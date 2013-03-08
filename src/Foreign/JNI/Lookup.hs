{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.JNI.Lookup (

  setJavaVM,

  jclass,
  jmethodid,
  jfieldid,

  ) where

import Foreign.JNI
import Foreign.Ptr (nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

vmref :: IORef JavaVM
vmref = unsafePerformIO $ newIORef nullPtr

setJavaVM :: JavaVM -> IO ()
setJavaVM = writeIORef vmref
foreign export ccall "hs_set_java_vm" setJavaVM :: JavaVM -> IO ()

jclass :: String -> JClass
jclass name = unsafePerformIO $ do
  readIORef vmref >>= getEnv >>= either
    (error $ "getEnv failed during " ++ name ++ " class lookup")
    (\e -> runJNI e $ findClass name >>= newGlobalRef)

jmethodid :: JClass -> String -> String -> JMethodID
jmethodid cls name sig = unsafePerformIO $ do
  readIORef vmref >>= getEnv >>= either
    (error $ "getEnv failed during " ++ name ++ " " ++ sig ++ " method lookup")
    (\e -> runJNI e $ getMethodID cls name sig)

jfieldid :: JClass -> String -> String -> JFieldID
jfieldid cls name sig = unsafePerformIO $ do
  readIORef vmref >>= getEnv >>= either
    (error $ "getEnv failed during " ++ name ++ " " ++ sig ++ " field lookup")
    (\e -> runJNI e $ getFieldID cls name sig)
