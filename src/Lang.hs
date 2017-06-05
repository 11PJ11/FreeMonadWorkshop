module Lang where

import           Control.Monad.Free
import           System.Exit        hiding (ExitSuccess)


-- output b -- prints a "b" to the console
-- bell     -- rings the computer's bell
-- done     -- end of execution
data Toy b next = Output b next
                | Bell next
                | Done

aFunc = Bell (Output 'A' Done)

-- With Cheat we've defined a stream of functors that will only end when it gets
-- to the Done constructor.
data Cheat f = Cheat (f (Cheat f))

-- Fortunately, Cheat already exists in Haskell and goes by another name Fix.
-- It's named Fix because it is "the fixed point of a functor".
-- It's like cons for functors
-- This approach only works if you can use the Done constructor to
--    terminate every chain of functors.
-- Unfortunately, programmers don't often have the luxury of
--    writing the entire program from start to finish.
-- We often just want to write subroutines that can be called
--    from within other programs and our Fix trick doesn't let us write a
--    subroutine without terminating the entire program.
-- data Fix f = Fix (f (Fix f))
aFixFunc =
   -- Fix (Output 'A' (Fix Done))
   Fix (Bell (Fix (Output 'A'(Fix Done))))

-- Our subroutine finished but we are not ready to call Done,
--    so instead we throw an exception and let whoever calls our subroutine
--    catch it and resume from where we left off
data FixE f e = Fix (f (FixE f e))
              | Throw e

catch ::(Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f   = Fix (fmap (`catch` f) x) -- ==> Fix (f (catch x)), bind
catch (Throw e) f = f e -- return

-- This only works with functors so:
instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

-- FixE already exists and it's called the Free monad:
-- data Free f r = Free (f (Free f r)) | Pure r

-- instance (Functor f) => Monad (Free f) where
--     return = Pure
--     (Free x) >>= f = Free (fmap (>>= f) x)
--     (Pure r) >>= f = f r

-- Free (Toy b) is the monad, not Toy b, which means that if
--    we want to sequence our primitive commands using do notation,
--    we have convert our commands of type Toy b into Free (Toy b)

-- Pure at the end allows for future binding
-- this way we can combine other Free monads
-- after this
output :: a -> Free (Toy a) ()
-- output x = Free (Output x (Pure ()))

bell :: Free (Toy a) ()
-- bell = Free (Bell (Pure ()))

done :: Free (Toy a) r
-- Done is the end of the computation and has no Pure
-- at the end
-- done = Free Done

-- with liftF we can now abstract the operation of moving our type
-- into the Free Monad world
-- liftF :: (Functor f) => f r -> Free f r
-- liftF command = Free (fmap Pure command)

output x = liftF (Output x ())
bell     = liftF (Bell     ())
done     = liftF  Done

-- The followng is not a program IT IS A DATA TYPE
program :: Free (Toy Char) r
program = do
    output 'A'
    bell
    done

-- let's define our first INTERPRETER for this data type
-- a PRETTY PRINTER
showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"

data Direction = North
               | South
               | West
               | East

data Image = Image [Int]

-- these are types NOT functions
-- these types describe interactions
-- but ARE NOT interactions themselves
data Interaction next = Look Direction (Image -> next)
                      | Fire Direction next
                      | ReadLine (String -> next)
                      | WriteLine String (Bool -> next)

instance Functor Interaction where
    fmap f (Look dir g)    = Look dir (f . g)
    fmap f (Fire dir x)    = Fire dir (f x)
    fmap f (ReadLine g)    = ReadLine (f . g)
    fmap f (WriteLine s g) = WriteLine s (f . g)

type Program = Free Interaction

look :: Direction -> Program Image
look dir = liftF (Look dir id)

fire :: Direction -> Program ()
fire dir = liftF (Fire dir ())

readLine :: Program String
readLine = liftF (ReadLine id)

writeLine :: String -> Program Bool
writeLine s = liftF (WriteLine s id)



-- interpret :: Program r -> Game r
-- interpret prog = case prog of
--     Free (Look dir g) -> do
--         img <- collectImage dir
--         interpret (g img)
--     Free (Fire dir next) -> do
--         sendBullet dir
--         interpret next
--     Free (ReadLine g) -> do
--         str <- getChatLine
--         interpret (g str)
--     Free (WriteLine s g) ->
--         putChatLine s
--         interpret (g True)
--     Pure r -> return r



data TeletypeF next
  = PutStrLn String next
  | GetLine (String -> next)
  | ExitSuccess

instance Functor TeletypeF where
    fmap f (PutStrLn str next) = PutStrLn str (f next)
    fmap f (GetLine      g)    = GetLine (f . g)
    fmap f  ExitSuccess        = ExitSuccess

type Teletype = Free TeletypeF

newPutStrLn :: String -> Teletype ()
newPutStrLn str = liftF $ PutStrLn str ()

newGetLine :: Teletype String
newGetLine = liftF $ GetLine id

newExitSuccess :: Teletype r
newExitSuccess = liftF ExitSuccess

run :: Teletype r -> IO r
run (Pure r)                = return r
run (Free (PutStrLn str t)) = putStrLn str >>  run t
run (Free (GetLine  f    )) = getLine      >>= run . f
run (Free  ExitSuccess    ) = exitSuccess

echo :: Teletype ()
echo = do
   str <- newGetLine
   newPutStrLn str
   newPutStrLn "Finished"
   newExitSuccess
