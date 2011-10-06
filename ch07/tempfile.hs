import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catch)
import Control.Exception(finally)

-- main entry. work with a temp file in myAction
main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
  do -- Start by displaying a greeting on the term
    putStrLn "Welcome to tempfile.hs"
    putStrLn $ "I have a temp file at " ++ tempname
    
    -- Let's see where init pos is
    pos <- hTell temph
    putStrLn $ "My initial pos is " ++ show pos

    -- Now write some data to the temp
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++
               show (length tempdata) ++ " bytes: " ++
               tempdata
    hPutStrLn temph tempdata

    -- get out new pos. this doesn't actually modify pos
    -- in memory, but makes the name pos correspond to
    -- a different value for the remainder of the do block
    pos <- hTell temph
    putStrLn $ "After writing, my new pos is " ++ show pos

    -- Seek to the beginning of the file and display
    putStrLn $ "The file content is "
    hSeek temph AbsoluteSeek 0

    -- hGetContents performs lazy read entire file
    c <- hGetContents temph

    -- copy the file byte for byte to stdout, follow
    putStrLn c

    -- let's also display it as a haskell literal
    putStrLn $ "Which could be expressed as this hs lit: "
    print c

-- this func takes two params: a filename pattern and another
-- function. it will create a temp file, and pass the name and handle
-- of that file to the given function.
--
-- the temp file is created with openTempFile. the directory is the one
-- indicated by getTemporaryDiretory, or, if the system has no notion of
-- a temporary directory, "." is used. the given pattern is passed to 
-- openTempFile.
--
-- after the given fucntion terminates, even if it terminates due to an
-- exception, the handle is closed and the file is deleted
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
  do -- the library ref says that getTemporaryDirectory may raise an
     -- exception on systems that have no notion of a temp dir.
     -- so we run it under a catch. catch takes two functions, one
     -- to run and a different one to run if exception occurs. 
     -- if function raised an exception, just use ".".
     tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
     (tempfile,temph) <- openTempFile tempdir pattern

     -- Call (func tempfile temph) to perform the action on the temp
     -- file. finally takes two actions. the first is to run. the second
     -- is an action to run after the first, regardless of whether the first
     -- raised an exception. this way we ensure the temp file
     -- is always deleted. the return value from finally is the first
     -- action's return value.
     finally (func tempfile temph)
             (do hClose temph
                 removeFile tempfile)
