{-# LANGUAGE ForeignFunctionInterface, CPP, EmptyDataDecls, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Foreign.JNI (

-- Types and Data Structures
  JObject,
  JClass,
  JString,
  JThrowable,
  JWeak,
  JMethodID,
  JFieldID,

  JInt,
  JBoolean,
  JByte,
  JChar,
  JShort,
  JLong,
  JFloat,
  JDouble,
  JSize,

  JArray,
  JObjectArray,
  JBooleanArray,
  JByteArray,
  JCharArray,
  JShortArray,
  JIntArray,
  JLongArray,
  JFloatArray,
  JDoubleArray,

  JValue(..),
  IsJValue(..),
  JObjectRefType(..),
  JNIError(..),
  JavaException(..),
  NativeMethod(..),
  fromJBoolean,

  JNIEnv,
  JavaVM,

-- Monad
  JNI,
  runJNI,
  runJNISafe,

-- Version Information
  getVersion,

-- Class Operations
  defineClass,
  findClass,
  getSuperclass,
  isAssignable,

-- Exceptions
  throw,
  throwNew,
  exceptionOccurred,
  exceptionDescribe,
  exceptionClear,
  fatalError,
  exceptionCheck,

-- Global and Local References
  newGlobalRef,
  deleteGlobalRef,
  deleteLocalRef,
  ensureLocalCapacity,
  pushLocalFrame,
  popLocalFrame,
  newLocalRef,

-- Weak Global References
  newWeakGlobalRef,
  deleteWeakGlobalRef,

-- Object Operations
  allocObject,
  newObject,
  getObjectClass,
  getObjectRefType,
  isInstanceOf,
  isSameObject,

-- Accessing Fields of Objects
  getFieldID,

  getObjectField,
  getBooleanField,
  getByteField,
  getCharField,
  getShortField,
  getIntField,
  getLongField,
  getFloatField,
  getDoubleField,
  GetField(..),

  setObjectField,
  setBooleanField,
  setByteField,
  setCharField,
  setShortField,
  setIntField,
  setLongField,
  setFloatField,
  setDoubleField,
  SetField(..),

-- Calling Instance Methods
  getMethodID,

  callVoidMethod,
  callObjectMethod,
  callBooleanMethod,
  callByteMethod,
  callCharMethod,
  callShortMethod,
  callIntMethod,
  callLongMethod,
  callFloatMethod,
  callDoubleMethod,
  CallMethod(..),

  callNonvirtualVoidMethod,
  callNonvirtualObjectMethod,
  callNonvirtualBooleanMethod,
  callNonvirtualByteMethod,
  callNonvirtualCharMethod,
  callNonvirtualShortMethod,
  callNonvirtualIntMethod,
  callNonvirtualLongMethod,
  callNonvirtualFloatMethod,
  callNonvirtualDoubleMethod,
  CallNonvirtualMethod(..),

-- Accessing Static Fields
  getStaticFieldID,

  getStaticObjectField,
  getStaticBooleanField,
  getStaticByteField,
  getStaticCharField,
  getStaticShortField,
  getStaticIntField,
  getStaticLongField,
  getStaticFloatField,
  getStaticDoubleField,
  GetStaticField(..),

  setStaticObjectField,
  setStaticBooleanField,
  setStaticByteField,
  setStaticCharField,
  setStaticShortField,
  setStaticIntField,
  setStaticLongField,
  setStaticFloatField,
  setStaticDoubleField,
  SetStaticField(..),

-- Calling Static Methods
  getStaticMethodID,

  callStaticVoidMethod,
  callStaticObjectMethod,
  callStaticBooleanMethod,
  callStaticByteMethod,
  callStaticCharMethod,
  callStaticShortMethod,
  callStaticIntMethod,
  callStaticLongMethod,
  callStaticFloatMethod,
  callStaticDoubleMethod,
  CallStaticMethod(..),

-- String Operations
  newString,
  fromJString,
  getStringLength,
  getStringChars,
  releaseStringChars,
  newStringUTF,
  getStringUTFLength,
  getStringUTFChars,
  releaseStringUTFChars,
  getStringRegion,
  getStringUTFRegion,
  getStringCritical,
  releaseStringCritical,

-- Array Operations
  getArrayLength,
  newObjectArray,
  getObjectArrayElement,
  setObjectArrayElement,

  newBooleanArray,
  newByteArray,
  newCharArray,
  newShortArray,
  newIntArray,
  newLongArray,
  newFloatArray,
  newDoubleArray,

  getBooleanArrayElements,
  getByteArrayElements,
  getCharArrayElements,
  getShortArrayElements,
  getIntArrayElements,
  getLongArrayElements,
  getFloatArrayElements,
  getDoubleArrayElements,

  releaseBooleanArrayElements,
  releaseByteArrayElements,
  releaseCharArrayElements,
  releaseShortArrayElements,
  releaseIntArrayElements,
  releaseLongArrayElements,
  releaseFloatArrayElements,
  releaseDoubleArrayElements,

  getBooleanArrayRegion,
  getByteArrayRegion,
  getCharArrayRegion,
  getShortArrayRegion,
  getIntArrayRegion,
  getLongArrayRegion,
  getFloatArrayRegion,
  getDoubleArrayRegion,

  setBooleanArrayRegion,
  setByteArrayRegion,
  setCharArrayRegion,
  setShortArrayRegion,
  setIntArrayRegion,
  setLongArrayRegion,
  setFloatArrayRegion,
  setDoubleArrayRegion,

  getPrimitiveArrayCritical,
  releasePrimitiveArrayCritical,

-- Registering Native Methods
  toNativeMethodPtr,
  registerNatives,
  unregisterNatives,

-- Monitor Operations
  monitorEnter,
  monitorExit,

-- NIO Support
  newDirectByteBuffer,
  getDirectBufferAddress,
  getDirectBufferCapacity,

-- Reflection Support
  fromReflectedMethod,
  fromReflectedField,
  toReflectedMethod,
  toReflectedField,

-- Java VM Interface
  getJavaVM,

-- Invocation API Functions
  -- jint JNI_GetDefaultJavaVMInitArgs(void *vm_args);
  -- jint JNI_GetCreatedJavaVMs(JavaVM **vmBuf, jsize bufLen, jsize *nVMs);
  -- jint JNI_CreateJavaVM(JavaVM **p_vm, void **p_env, void *vm_args);
  destroyJavaVM,
  attachCurrentThread,
  attachCurrentThreadAsDaemon,
  detachCurrentThread,
  getEnv,
  ) where

import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Word (Word16)
import Data.Text (Text)
import Data.Text.Foreign
import Data.Typeable (Typeable)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Applicative
import qualified Control.Exception (Exception, throw, catch)

data JObjectObj
type JObject = Ptr JObjectObj
type JClass = Ptr JObjectObj
type JString = Ptr JObjectObj
type JThrowable = Ptr JObjectObj
data JWeakObj
type JWeak = Ptr JWeakObj
data JMethodIDObj
type JMethodID = Ptr JMethodIDObj
data JFieldIDObj
type JFieldID = Ptr JFieldIDObj

type JInt = CInt
type JBoolean = CUChar
type JByte = CChar
type JChar = Word16
type JShort = CShort
type JLong = CLong
type JFloat = CFloat
type JDouble = CDouble
type JSize = JInt

type JArray = Ptr JObjectObj
type JObjectArray = Ptr JObjectObj
type JBooleanArray = Ptr JObjectObj
type JByteArray = Ptr JObjectObj
type JCharArray = Ptr JObjectObj
type JShortArray = Ptr JObjectObj
type JIntArray = Ptr JObjectObj
type JLongArray = Ptr JObjectObj
type JFloatArray = Ptr JObjectObj
type JDoubleArray = Ptr JObjectObj

data JValue = JVObject JObject
        | JVInt JInt
        | JVBool JBoolean
        | JVByte JByte
        | JVChar JChar
        | JVShort JShort
        | JVLong JLong
        | JVFloat JFloat
        | JVDouble JDouble

data JObjectRefType = JNIInvalidRefType | JNILocalRefType | JNIGlobalRefType | JNIWeakGlobalRefType
data JNIError = JNI_OK | JNI_ERR | JNI_EDETACHED | JNI_EVERSION

data JavaException = JavaExceptionPending deriving (Show, Typeable)
instance Control.Exception.Exception JavaException

#include "jni.h"
#ifdef linux_android_HOST_OS
#define JNINativeInterface_ JNINativeInterface
#define JNIInvokeInterface_ JNIInvokeInterface
#endif

fromJBoolean :: JBoolean -> Bool
fromJBoolean (#{const JNI_FALSE}) = False
fromJBoolean _ = True

toJNIError :: JInt -> JNIError
toJNIError (#{const JNI_OK}) = JNI_OK
toJNIError (#{const JNI_EDETACHED}) = JNI_EDETACHED
toJNIError (#{const JNI_EVERSION}) = JNI_EVERSION
toJNIError _ = JNI_ERR

instance Storable JValue where
  sizeOf _ = #{size jvalue}
  alignment _ = #{size jvalue}
  peek _ptr = undefined
  poke ptr (JVObject v) =  poke (castPtr ptr :: Ptr JObject) v
  poke ptr (JVInt v) =  poke (castPtr ptr :: Ptr JInt) v
  poke ptr (JVBool v) = poke (castPtr ptr :: Ptr JBoolean) v
  poke ptr (JVByte v) = poke (castPtr ptr :: Ptr JByte) v
  poke ptr (JVChar v) = poke (castPtr ptr :: Ptr JChar) v
  poke ptr (JVShort v) = poke (castPtr ptr :: Ptr JShort) v
  poke ptr (JVLong v) = poke (castPtr ptr :: Ptr JLong) v
  poke ptr (JVFloat v) = poke (castPtr ptr :: Ptr JFloat) v
  poke ptr (JVDouble v) = poke (castPtr ptr :: Ptr JDouble) v

class IsJValue a where jv :: a -> JValue
instance IsJValue JObject where jv v = JVObject v
instance IsJValue JInt where jv v = JVInt v
instance IsJValue JBoolean where jv v = JVBool v
instance IsJValue JByte where jv v = JVByte v
instance IsJValue JChar where jv v = JVChar v
instance IsJValue JShort where jv v = JVShort v
instance IsJValue JLong where jv v = JVLong v
instance IsJValue JFloat where jv v = JVFloat v
instance IsJValue JDouble where jv v = JVDouble v

data JNINativeInterface_
type JNIEnv = Ptr (Ptr JNINativeInterface_)
data JNIInvokeInterface_
type JavaVM = Ptr (Ptr JNIInvokeInterface_)

newtype JNI a = JNI_ {
  runJNI_ :: ReaderT JNIEnv IO a
} deriving (Monad, MonadIO, MonadReader JNIEnv)

runJNI :: JNIEnv -> JNI a -> IO a
runJNI e k = runReaderT (runJNI_ k) e

runJNISafe :: a -> JNIEnv -> JNI a -> IO a
runJNISafe v e k = Control.Exception.catch (runReaderT (runJNI_ k) e) (noop v)
  where
    noop :: a -> JavaException -> IO a
    noop v' _ = return v'

withEnv :: (JNIEnv -> Ptr JNINativeInterface_ -> IO a) -> JNI a
withEnv f = do
  e <- ask
  liftIO $ do
    p <- peek e
    a <- f e p
    pending <- exceptionCheckIO e p
    if fromJBoolean pending
      then Control.Exception.throw JavaExceptionPending
      else return a

withEnvUnchecked :: (JNIEnv -> Ptr JNINativeInterface_ -> IO a) -> JNI a
withEnvUnchecked f = do
  e <- ask
  liftIO (peek e >>= f e)

-- Version Information

-- jint GetVersion(JNIEnv *env);
foreign import ccall "dynamic" mkGetVersion ::
  FunPtr (JNIEnv -> IO JInt) -> JNIEnv -> IO JInt
getVersion :: JNI JInt
getVersion = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetVersion} p)
  mkGetVersion f e

-- Class Operations

-- jclass DefineClass(JNIEnv *env, const char *name, jobject loader, const jbyte *buf, jsize bufLen);
foreign import ccall "dynamic" mkDefineClass ::
  FunPtr (JNIEnv -> CString -> JObject -> Ptr JByte -> JSize -> IO JClass) ->  (JNIEnv -> CString -> JObject -> Ptr JByte -> JSize -> IO JClass)
defineClass :: CString -> JObject -> Ptr JByte -> JSize -> JNI JClass
defineClass name loader buf bufLen = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, DefineClass} p)
  mkDefineClass f e name loader buf bufLen

-- jclass FindClass(JNIEnv *env, const char *name);
foreign import ccall "dynamic" mkFindClass ::
  FunPtr (JNIEnv -> CString -> IO JClass) -> JNIEnv -> CString -> IO JClass
findClass :: String -> JNI JClass
findClass s = withEnv $ \e p -> withCString s $ \cs -> do
  f <- (#{peek struct JNINativeInterface_, FindClass} p)
  mkFindClass f e cs

-- jclass GetSuperclass(JNIEnv *env, jclass clazz);
foreign import ccall "dynamic" mkGetSuperclass ::
  FunPtr (JNIEnv -> JClass -> IO JClass) -> JNIEnv -> JClass -> IO JClass
getSuperclass :: JClass -> JNI JClass
getSuperclass cls = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetSuperclass} p)
  mkGetSuperclass f e cls

-- jboolean IsAssignableFrom(JNIEnv *env, jclass clazz1, jclass clazz2);
foreign import ccall "dynamic" mkIsAssignableFrom ::
  FunPtr (JNIEnv -> JClass -> JClass -> IO JBoolean) -> JNIEnv -> JClass -> JClass -> IO JBoolean
isAssignable :: JClass -> JClass -> JNI JBoolean
isAssignable c1 c2 = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, IsAssignableFrom} p)
  mkIsAssignableFrom f e c1 c2

-- Exceptions

-- jint Throw(JNIEnv *env, jthrowable obj);
foreign import ccall "dynamic" mkThrow ::
  FunPtr (JNIEnv -> JThrowable -> IO JInt) -> JNIEnv -> JThrowable -> IO JInt
throw :: JThrowable -> JNI JNIError
throw err = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, Throw} p)
  mkThrow f e err >>= return . toJNIError

-- jint ThrowNew(JNIEnv *env, jclass clazz, const char *message);
foreign import ccall "dynamic" mkThrowNew ::
  FunPtr (JNIEnv -> JClass -> CString -> IO JInt) -> JNIEnv -> JClass -> CString -> IO JInt
throwNew :: JClass -> String -> JNI JNIError
throwNew cls s = withEnv $ \e p -> withCString s $ \ cs -> do
  f <- (#{peek struct JNINativeInterface_, ThrowNew} p)
  mkThrowNew f e cls cs >>= return . toJNIError

-- jthrowable ExceptionOccurred(JNIEnv *env);
foreign import ccall "dynamic" mkExceptionOccurred ::
  FunPtr (JNIEnv -> IO JThrowable) -> JNIEnv -> IO JThrowable
exceptionOccurred :: JNI JThrowable
exceptionOccurred = withEnvUnchecked $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ExceptionOccurred} p)
  mkExceptionOccurred f e

-- void ExceptionDescribe(JNIEnv *env);
foreign import ccall "dynamic" mkExceptionDescribe ::
  FunPtr (JNIEnv -> IO ()) -> JNIEnv -> IO ()
exceptionDescribe :: JNI ()
exceptionDescribe = withEnvUnchecked $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ExceptionDescribe} p)
  mkExceptionDescribe f e

-- void ExceptionClear(JNIEnv *env);
foreign import ccall "dynamic" mkExceptionClear ::
  FunPtr (JNIEnv -> IO ()) -> JNIEnv -> IO ()
exceptionClear :: JNI ()
exceptionClear = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ExceptionClear} p)
  mkExceptionClear f e

-- void FatalError(JNIEnv *env, const char *msg);
foreign import ccall "dynamic" mkFatalError ::
  FunPtr (JNIEnv -> CString -> IO ()) -> JNIEnv -> CString -> IO ()
fatalError :: String -> JNI ()
fatalError s = withEnv $ \e p -> withCString s $ \cs -> do
  f <- (#{peek struct JNINativeInterface_, FatalError} p)
  mkFatalError f e cs

-- jboolean ExceptionCheck(JNIEnv *env);
foreign import ccall "dynamic" mkExceptionCheck ::
  FunPtr (JNIEnv -> IO JBoolean) -> JNIEnv -> IO JBoolean
exceptionCheckIO :: JNIEnv -> Ptr JNINativeInterface_ -> IO JBoolean
exceptionCheckIO e p = do
  f <- (#{peek struct JNINativeInterface_, ExceptionCheck} p)
  mkExceptionCheck f e

exceptionCheck :: JNI JBoolean
exceptionCheck = withEnvUnchecked exceptionCheckIO

-- Global and Local References

-- jobject NewGlobalRef(JNIEnv *env, jobject obj);
foreign import ccall "dynamic" mkNewGlobalRef ::
  FunPtr (JNIEnv -> JObject -> IO JObject) -> JNIEnv -> JObject -> IO JObject
newGlobalRef :: JObject -> JNI JObject
newGlobalRef o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewGlobalRef} p)
  mkNewGlobalRef f e o

-- void DeleteGlobalRef(JNIEnv *env, jobject globalRef);
foreign import ccall "dynamic" mkDeleteGlobalRef ::
  FunPtr (JNIEnv -> JObject -> IO ()) -> JNIEnv -> JObject -> IO ()
deleteGlobalRef :: JObject -> JNI ()
deleteGlobalRef o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, DeleteGlobalRef} p)
  mkDeleteGlobalRef f e o

-- void DeleteLocalRef(JNIEnv *env, jobject localRef);
foreign import ccall "dynamic" mkDeleteLocalRef ::
  FunPtr (JNIEnv -> JObject -> IO ()) -> JNIEnv -> JObject -> IO()
deleteLocalRef :: JObject -> JNI ()
deleteLocalRef o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, DeleteLocalRef} p)
  mkDeleteLocalRef f e o

-- jint EnsureLocalCapacity(JNIEnv *env, jint capacity);
foreign import ccall "dynamic" mkEnsureLocalCapacity ::
  FunPtr (JNIEnv -> JInt -> IO JInt) -> JNIEnv -> JInt -> IO JInt
ensureLocalCapacity :: JInt -> JNI JNIError
ensureLocalCapacity cap = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, EnsureLocalCapacity} p)
  mkEnsureLocalCapacity f e cap >>= return . toJNIError

-- jint PushLocalFrame(JNIEnv *env, jint capacity);
foreign import ccall "dynamic" mkPushLocalFrame ::
  FunPtr (JNIEnv -> JInt -> IO JInt) -> JNIEnv -> JInt -> IO JInt
pushLocalFrame :: JInt -> JNI JNIError
pushLocalFrame cap = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, PushLocalFrame} p)
  mkPushLocalFrame f e cap >>= return . toJNIError

-- jobject PopLocalFrame(JNIEnv *env, jobject result);
foreign import ccall "dynamic" mkPopLocalFrame ::
  FunPtr (JNIEnv -> JObject -> IO JObject) -> JNIEnv -> JObject -> IO JObject
popLocalFrame :: JObject -> JNI JObject
popLocalFrame o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, PopLocalFrame} p)
  mkPopLocalFrame f e o

-- jobject NewLocalRef(JNIEnv *env, jobject ref);
foreign import ccall "dynamic" mkNewLocalRef ::
  FunPtr (JNIEnv -> JObject -> IO JObject) -> JNIEnv -> JObject -> IO JObject
newLocalRef :: JObject -> JNI JObject
newLocalRef o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewLocalRef} p)
  mkNewLocalRef f e o

-- Weak Global References

-- jweak NewWeakGlobalRef(JNIEnv *env, jobject obj);
foreign import ccall "dynamic" mkNewWeakGlobalRef ::
  FunPtr (JNIEnv -> JObject -> IO JWeak) -> JNIEnv -> JObject -> IO JWeak
newWeakGlobalRef :: JObject -> JNI JWeak
newWeakGlobalRef o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewWeakGlobalRef} p)
  mkNewWeakGlobalRef f e o

-- void DeleteWeakGlobalRef(JNIEnv *env, jweak obj);
foreign import ccall "dynamic" mkDeleteWeakGlobalRef ::
  FunPtr (JNIEnv -> JWeak -> IO ()) -> JNIEnv -> JWeak -> IO ()
deleteWeakGlobalRef :: JWeak -> JNI ()
deleteWeakGlobalRef o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, DeleteWeakGlobalRef} p)
  mkDeleteWeakGlobalRef f e o

-- Object Operations

-- jobject AllocObject(JNIEnv *env, jclass clazz);
foreign import ccall "dynamic" mkAllocObject ::
  FunPtr (JNIEnv -> JClass -> IO JObject) -> JNIEnv -> JClass -> IO JObject
allocObject :: JClass -> JNI JObject
allocObject cls = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, AllocObject} p)
  mkAllocObject f e cls

-- jobject NewObjectA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkNewObjectA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JObject) -> JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JObject
newObject :: JClass -> JMethodID -> [JValue] -> JNI JObject
newObject cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, NewObjectA} p)
  mkNewObjectA f e cls m argv

-- jclass GetObjectClass(JNIEnv *env, jobject obj);
foreign import ccall "dynamic" mkGetObjectClass ::
  FunPtr (JNIEnv -> JObject -> IO JClass) -> JNIEnv -> JObject -> IO JClass
getObjectClass :: JObject -> JNI JClass
getObjectClass o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetObjectClass} p)
  mkGetObjectClass f e o

-- jobjectRefType GetObjectRefType(JNIEnv* env, jobject obj);
foreign import ccall "dynamic" mkGetObjectRefType ::
  FunPtr (JNIEnv -> JObject -> IO JInt) -> JNIEnv -> JObject -> IO JInt
getObjectRefType :: JObject -> JNI JObjectRefType
getObjectRefType o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetObjectRefType} p)
  t <- mkGetObjectRefType f e o
  return $ case t of
    #{const JNILocalRefType} -> JNILocalRefType
    #{const JNIGlobalRefType} -> JNIGlobalRefType
    #{const JNIWeakGlobalRefType} -> JNIWeakGlobalRefType
    _ -> JNIInvalidRefType

-- jboolean IsInstanceOf(JNIEnv *env, jobject obj, jclass clazz);
foreign import ccall "dynamic" mkIsInstanceOf ::
  FunPtr (JNIEnv -> JObject -> JClass -> IO JBoolean) -> JNIEnv -> JObject -> JClass -> IO JBoolean
isInstanceOf :: JObject -> JClass -> JNI JBoolean
isInstanceOf o cls = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, IsInstanceOf} p)
  mkIsInstanceOf f e o cls

  -- jboolean IsSameObject(JNIEnv *env, jobject ref1, jobject ref2);
foreign import ccall "dynamic" mkIsSameObject ::
  FunPtr (JNIEnv -> JObject -> JObject -> IO JBoolean) -> JNIEnv -> JObject -> JObject -> IO JBoolean
isSameObject :: JObject -> JObject -> JNI JBoolean
isSameObject o1 o2 = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, IsSameObject} p)
  mkIsSameObject f e o1 o2

-- Accessing Fields of Objects

-- jfieldID GetFieldID(JNIEnv *env, jclass clazz, const char *name, const char *sig);
foreign import ccall "dynamic" mkGetFieldID ::
  FunPtr (JNIEnv -> JClass -> CString -> CString -> IO JFieldID) -> JNIEnv -> JClass -> CString -> CString -> IO JFieldID
getFieldID :: JClass -> String -> String -> JNI JFieldID
getFieldID cls name sig = withEnv $ \e p -> withCString name $ \cname -> withCString sig $ \csig -> do
  f <- (#{peek struct JNINativeInterface_, GetFieldID} p)
  mkGetFieldID f e cls cname csig

-- jobject GetObjectField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetObjectField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JObject) -> JNIEnv -> JObject -> JFieldID -> IO JObject
getObjectField :: JObject -> JFieldID -> JNI JObject
getObjectField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetObjectField} p)
  mkGetObjectField f e o fid

-- jboolean GetBooleanField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetBooleanField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JBoolean) -> JNIEnv -> JObject -> JFieldID -> IO JBoolean
getBooleanField :: JObject -> JFieldID -> JNI JBoolean
getBooleanField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetBooleanField} p)
  mkGetBooleanField f e o fid

-- jbyte GetByteField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetByteField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JByte) -> JNIEnv -> JObject -> JFieldID -> IO JByte
getByteField :: JObject -> JFieldID -> JNI JByte
getByteField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetByteField} p)
  mkGetByteField f e o fid

-- jchar GetCharField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetCharField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JChar) -> JNIEnv -> JObject -> JFieldID -> IO JChar
getCharField :: JObject -> JFieldID -> JNI JChar
getCharField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetCharField} p)
  mkGetCharField f e o fid

-- jshort GetShortField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetShortField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JShort) -> JNIEnv -> JObject -> JFieldID -> IO JShort
getShortField :: JObject -> JFieldID -> JNI JShort
getShortField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetShortField} p)
  mkGetShortField f e o fid

-- jint GetIntField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetIntField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JInt) -> JNIEnv -> JObject -> JFieldID -> IO JInt
getIntField :: JObject -> JFieldID -> JNI JInt
getIntField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetIntField} p)
  mkGetIntField f e o fid

-- jlong GetLongField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetLongField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JLong) -> JNIEnv -> JObject -> JFieldID -> IO JLong
getLongField :: JObject -> JFieldID -> JNI JLong
getLongField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetLongField} p)
  mkGetLongField f e o fid

-- jfloat GetFloatField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetFloatField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JFloat) -> JNIEnv -> JObject -> JFieldID -> IO JFloat
getFloatField :: JObject -> JFieldID -> JNI JFloat
getFloatField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetFloatField} p)
  mkGetFloatField f e o fid

-- jdouble GetDoubleField(JNIEnv *env, jobject obj, jfieldID fieldID)
foreign import ccall "dynamic" mkGetDoubleField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> IO JDouble) -> JNIEnv -> JObject -> JFieldID -> IO JDouble
getDoubleField :: JObject -> JFieldID -> JNI JDouble
getDoubleField o fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetDoubleField} p)
  mkGetDoubleField f e o fid

class GetField a where getField :: JObject -> JFieldID -> JNI a
instance GetField JObject where getField = getObjectField
instance GetField JBoolean where getField = getBooleanField
instance GetField JByte where getField = getByteField
instance GetField JChar where getField = getCharField
instance GetField JShort where getField = getShortField
instance GetField JInt where getField = getIntField
instance GetField JLong where getField = getLongField
instance GetField JFloat where getField = getFloatField
instance GetField JDouble where getField = getDoubleField

-- void SetObjectField(JNIEnv *env, jobject obj, jfieldID fieldID, jobject value)
foreign import ccall "dynamic" mkSetObjectField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JObject -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JObject -> IO ()
setObjectField :: JObject -> JFieldID -> JObject -> JNI ()
setObjectField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetObjectField} p)
  mkSetObjectField f e o fid v

-- void SetBooleanField(JNIEnv *env, jobject obj, jfieldID fieldID, jboolean value)
foreign import ccall "dynamic" mkSetBooleanField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JBoolean -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JBoolean -> IO ()
setBooleanField :: JObject -> JFieldID -> JBoolean -> JNI ()
setBooleanField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetBooleanField} p)
  mkSetBooleanField f e o fid v

-- void SetByteField(JNIEnv *env, jobject obj, jfieldID fieldID, jbyte value)
foreign import ccall "dynamic" mkSetByteField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JByte -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JByte -> IO ()
setByteField :: JObject -> JFieldID -> JByte -> JNI ()
setByteField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetByteField} p)
  mkSetByteField f e o fid v

-- void SetCharField(JNIEnv *env, jobject obj, jfieldID fieldID, jchar value)
foreign import ccall "dynamic" mkSetCharField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JChar -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JChar -> IO ()
setCharField :: JObject -> JFieldID -> JChar -> JNI ()
setCharField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetCharField} p)
  mkSetCharField f e o fid v

-- void SetShortField(JNIEnv *env, jobject obj, jfieldID fieldID, jshort value)
foreign import ccall "dynamic" mkSetShortField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JShort -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JShort -> IO ()
setShortField :: JObject -> JFieldID -> JShort -> JNI ()
setShortField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetShortField} p)
  mkSetShortField f e o fid v

-- void SetIntField(JNIEnv *env, jobject obj, jfieldID fieldID, jint value)
foreign import ccall "dynamic" mkSetIntField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JInt -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JInt -> IO ()
setIntField :: JObject -> JFieldID -> JInt -> JNI ()
setIntField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetIntField} p)
  mkSetIntField f e o fid v

-- void SetLongField(JNIEnv *env, jobject obj, jfieldID fieldID, jlong value)
foreign import ccall "dynamic" mkSetLongField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JLong -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JLong -> IO ()
setLongField :: JObject -> JFieldID -> JLong -> JNI ()
setLongField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetLongField} p)
  mkSetLongField f e o fid v

-- void SetFloatField(JNIEnv *env, jobject obj, jfieldID fieldID, jfloat value)
foreign import ccall "dynamic" mkSetFloatField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JFloat -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JFloat -> IO ()
setFloatField :: JObject -> JFieldID -> JFloat -> JNI ()
setFloatField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetFloatField} p)
  mkSetFloatField f e o fid v

-- void SetDoubleField(JNIEnv *env, jobject obj, jfieldID fieldID, jdouble value)
foreign import ccall "dynamic" mkSetDoubleField ::
  FunPtr (JNIEnv -> JObject -> JFieldID -> JDouble -> IO ()) -> JNIEnv -> JObject -> JFieldID -> JDouble -> IO ()
setDoubleField :: JObject -> JFieldID -> JDouble -> JNI ()
setDoubleField o fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetDoubleField} p)
  mkSetDoubleField f e o fid v

class SetField a where setField :: JObject -> JFieldID -> a -> JNI ()
instance SetField JObject where setField = setObjectField
instance SetField JBoolean where setField = setBooleanField
instance SetField JByte where setField = setByteField
instance SetField JChar where setField = setCharField
instance SetField JShort where setField = setShortField
instance SetField JInt where setField = setIntField
instance SetField JLong where setField = setLongField
instance SetField JFloat where setField = setFloatField
instance SetField JDouble where setField = setDoubleField

-- Calling Instance Methods

-- jmethodID GetMethodID(JNIEnv *env, jclass clazz, const char *name, const char *sig);
foreign import ccall "dynamic" mkGetMethodID ::
  FunPtr (JNIEnv -> JClass -> CString -> CString -> IO JMethodID) -> JNIEnv -> JClass -> CString -> CString -> IO JMethodID
getMethodID :: JClass -> String -> String -> JNI JMethodID
getMethodID cls name sig = withEnv $ \e p -> withCString name $ \cname -> withCString sig $ \csig -> do
  f <- (#{peek struct JNINativeInterface_, GetMethodID} p)
  mkGetMethodID f e cls cname csig

-- void CallVoidMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallVoidMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO ()) -> JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO ()
callVoidMethod :: JObject -> JMethodID -> [JValue] -> JNI ()
callVoidMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallVoidMethodA} p)
  mkCallVoidMethodA f e o m argv

-- jobject CallObjectMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallObjectMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JObject) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JObject
callObjectMethod :: JObject -> JMethodID -> [JValue] -> JNI JObject
callObjectMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallObjectMethodA} p)
  mkCallObjectMethodA f e o m argv

-- jboolean CallBooleanMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallBooleanMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JBoolean) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JBoolean
callBooleanMethod :: JObject -> JMethodID -> [JValue] -> JNI JBoolean
callBooleanMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallBooleanMethodA} p)
  mkCallBooleanMethodA f e o m argv

-- jbyte CallByteMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallByteMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JByte) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JByte
callByteMethod :: JObject -> JMethodID -> [JValue] -> JNI JByte
callByteMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallByteMethodA} p)
  mkCallByteMethodA f e o m argv

-- jchar CallCharMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallCharMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JChar) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JChar
callCharMethod :: JObject -> JMethodID -> [JValue] -> JNI JChar
callCharMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallCharMethodA} p)
  mkCallCharMethodA f e o m argv

-- jshort CallShortMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallShortMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JShort) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JShort
callShortMethod :: JObject -> JMethodID -> [JValue] -> JNI JShort
callShortMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallShortMethodA} p)
  mkCallShortMethodA f e o m argv

-- jint CallIntMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallIntMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JInt) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JInt
callIntMethod :: JObject -> JMethodID -> [JValue] -> JNI JInt
callIntMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallIntMethodA} p)
  mkCallIntMethodA f e o m argv

-- jlong CallLongMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallLongMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JLong) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JLong
callLongMethod :: JObject -> JMethodID -> [JValue] -> JNI JLong
callLongMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallLongMethodA} p)
  mkCallLongMethodA f e o m argv

-- jfloat CallFloatMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallFloatMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JFloat) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JFloat
callFloatMethod :: JObject -> JMethodID -> [JValue] -> JNI JFloat
callFloatMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallFloatMethodA} p)
  mkCallFloatMethodA f e o m argv

-- jdouble CallDoubleMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallDoubleMethodA ::
  FunPtr (JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JDouble) ->  JNIEnv -> JObject -> JMethodID -> Ptr JValue -> IO JDouble
callDoubleMethod :: JObject -> JMethodID -> [JValue] -> JNI JDouble
callDoubleMethod o m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallDoubleMethodA} p)
  mkCallDoubleMethodA f e o m argv

class CallMethod a where callMethod :: JObject -> JMethodID -> [JValue] -> JNI a
instance CallMethod () where callMethod = callVoidMethod
instance CallMethod JObject where callMethod = callObjectMethod
instance CallMethod JBoolean where callMethod = callBooleanMethod
instance CallMethod JByte where callMethod = callByteMethod
instance CallMethod JChar where callMethod = callCharMethod
instance CallMethod JShort where callMethod = callShortMethod
instance CallMethod JInt where callMethod = callIntMethod
instance CallMethod JLong where callMethod = callLongMethod
instance CallMethod JFloat where callMethod = callFloatMethod
instance CallMethod JDouble where callMethod = callDoubleMethod

-- void CallNonvirtualVoidMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualVoidMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO ()) -> JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO ()
callNonvirtualVoidMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI ()
callNonvirtualVoidMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualVoidMethodA} p)
  mkCallNonvirtualVoidMethodA f e o c m argv

-- jobject CallNonvirtualObjectMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualObjectMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JObject) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JObject
callNonvirtualObjectMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JObject
callNonvirtualObjectMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualObjectMethodA} p)
  mkCallNonvirtualObjectMethodA f e o c m argv

-- jboolean CallNonvirtualBooleanMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualBooleanMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JBoolean) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JBoolean
callNonvirtualBooleanMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JBoolean
callNonvirtualBooleanMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualBooleanMethodA} p)
  mkCallNonvirtualBooleanMethodA f e o c m argv

-- jbyte CallNonvirtualByteMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualByteMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JByte) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JByte
callNonvirtualByteMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JByte
callNonvirtualByteMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualByteMethodA} p)
  mkCallNonvirtualByteMethodA f e o c m argv

-- jchar CallNonvirtualCharMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualCharMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JChar) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JChar
callNonvirtualCharMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JChar
callNonvirtualCharMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualCharMethodA} p)
  mkCallNonvirtualCharMethodA f e o c m argv

-- jshort CallNonvirtualShortMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualShortMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JShort) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JShort
callNonvirtualShortMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JShort
callNonvirtualShortMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualShortMethodA} p)
  mkCallNonvirtualShortMethodA f e o c m argv

-- jint CallNonvirtualIntMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualIntMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JInt) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JInt
callNonvirtualIntMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JInt
callNonvirtualIntMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualIntMethodA} p)
  mkCallNonvirtualIntMethodA f e o c m argv

-- jlong CallNonvirtualLongMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualLongMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JLong) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JLong
callNonvirtualLongMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JLong
callNonvirtualLongMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualLongMethodA} p)
  mkCallNonvirtualLongMethodA f e o c m argv

-- jfloat CallNonvirtualFloatMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualFloatMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JFloat) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JFloat
callNonvirtualFloatMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JFloat
callNonvirtualFloatMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualFloatMethodA} p)
  mkCallNonvirtualFloatMethodA f e o c m argv

-- jdouble CallNonvirtualDoubleMethodA(JNIEnv *env, jobject obj, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallNonvirtualDoubleMethodA ::
  FunPtr (JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JDouble) ->  JNIEnv -> JObject -> JClass -> JMethodID -> Ptr JValue -> IO JDouble
callNonvirtualDoubleMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI JDouble
callNonvirtualDoubleMethod o c m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallNonvirtualDoubleMethodA} p)
  mkCallNonvirtualDoubleMethodA f e o c m argv

class CallNonvirtualMethod a where callNonvirtualMethod :: JObject -> JClass -> JMethodID -> [JValue] -> JNI a
instance CallNonvirtualMethod () where callNonvirtualMethod = callNonvirtualVoidMethod
instance CallNonvirtualMethod JObject where callNonvirtualMethod = callNonvirtualObjectMethod
instance CallNonvirtualMethod JBoolean where callNonvirtualMethod = callNonvirtualBooleanMethod
instance CallNonvirtualMethod JByte where callNonvirtualMethod = callNonvirtualByteMethod
instance CallNonvirtualMethod JChar where callNonvirtualMethod = callNonvirtualCharMethod
instance CallNonvirtualMethod JShort where callNonvirtualMethod = callNonvirtualShortMethod
instance CallNonvirtualMethod JInt where callNonvirtualMethod = callNonvirtualIntMethod
instance CallNonvirtualMethod JLong where callNonvirtualMethod = callNonvirtualLongMethod
instance CallNonvirtualMethod JFloat where callNonvirtualMethod = callNonvirtualFloatMethod
instance CallNonvirtualMethod JDouble where callNonvirtualMethod = callNonvirtualDoubleMethod

-- Accessing Static Fields

-- jfieldID GetStaticFieldID(JNIEnv *env, jclass clazz, const char *name, const char *sig);
foreign import ccall "dynamic" mkGetStaticFieldID ::
  FunPtr (JNIEnv -> JClass -> CString -> CString -> IO JFieldID) -> JNIEnv -> JClass -> CString -> CString -> IO JFieldID
getStaticFieldID :: JClass -> String -> String -> JNI JFieldID
getStaticFieldID cls name sig = withEnv $ \e p -> withCString name $ \cname -> withCString sig $ \csig -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticFieldID} p)
  mkGetStaticFieldID f e cls cname csig

-- jobject GetStaticObjectField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticObjectField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JObject) -> JNIEnv -> JClass -> JFieldID -> IO JObject
getStaticObjectField :: JClass -> JFieldID -> JNI JObject
getStaticObjectField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticObjectField} p)
  mkGetStaticObjectField f e cls fid

-- jboolean GetStaticBooleanField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticBooleanField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JBoolean) -> JNIEnv -> JClass -> JFieldID -> IO JBoolean
getStaticBooleanField :: JClass -> JFieldID -> JNI JBoolean
getStaticBooleanField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticBooleanField} p)
  mkGetStaticBooleanField f e cls fid

-- jbyte GetStaticByteField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticByteField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JByte) -> JNIEnv -> JClass -> JFieldID -> IO JByte
getStaticByteField :: JClass -> JFieldID -> JNI JByte
getStaticByteField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticByteField} p)
  mkGetStaticByteField f e cls fid

-- jchar GetStaticCharField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticCharField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JChar) -> JNIEnv -> JClass -> JFieldID -> IO JChar
getStaticCharField :: JClass -> JFieldID -> JNI JChar
getStaticCharField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticCharField} p)
  mkGetStaticCharField f e cls fid

-- jshort GetStaticShortField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticShortField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JShort) -> JNIEnv -> JClass -> JFieldID -> IO JShort
getStaticShortField :: JClass -> JFieldID -> JNI JShort
getStaticShortField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticShortField} p)
  mkGetStaticShortField f e cls fid

-- jint GetStaticIntField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticIntField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JInt) -> JNIEnv -> JClass -> JFieldID -> IO JInt
getStaticIntField :: JClass -> JFieldID -> JNI JInt
getStaticIntField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticIntField} p)
  mkGetStaticIntField f e cls fid

-- jlong GetStaticLongField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticLongField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JLong) -> JNIEnv -> JClass -> JFieldID -> IO JLong
getStaticLongField :: JClass -> JFieldID -> JNI JLong
getStaticLongField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticLongField} p)
  mkGetStaticLongField f e cls fid

-- jfloat GetStaticFloatField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticFloatField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JFloat) -> JNIEnv -> JClass -> JFieldID -> IO JFloat
getStaticFloatField :: JClass -> JFieldID -> JNI JFloat
getStaticFloatField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticFloatField} p)
  mkGetStaticFloatField f e cls fid

-- jdouble GetStaticDoubleField(JNIEnv *env, jclass clazz, jfieldID fieldID);
foreign import ccall "dynamic" mkGetStaticDoubleField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> IO JDouble) -> JNIEnv -> JClass -> JFieldID -> IO JDouble
getStaticDoubleField :: JClass -> JFieldID -> JNI JDouble
getStaticDoubleField cls fid = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticDoubleField} p)
  mkGetStaticDoubleField f e cls fid

class GetStaticField a where getStaticField :: JClass -> JFieldID -> JNI a
instance GetStaticField JObject where getStaticField = getStaticObjectField
instance GetStaticField JBoolean where getStaticField = getStaticBooleanField
instance GetStaticField JByte where getStaticField = getStaticByteField
instance GetStaticField JChar where getStaticField = getStaticCharField
instance GetStaticField JShort where getStaticField = getStaticShortField
instance GetStaticField JInt where getStaticField = getStaticIntField
instance GetStaticField JLong where getStaticField = getStaticLongField
instance GetStaticField JFloat where getStaticField = getStaticFloatField
instance GetStaticField JDouble where getStaticField = getStaticDoubleField

-- void SetStaticObjectField(JNIEnv *env, jclass clazz, jfieldID fieldID, jobject value);
foreign import ccall "dynamic" mkSetStaticObjectField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JObject -> IO()) -> JNIEnv -> JClass -> JFieldID -> JObject -> IO ()
setStaticObjectField :: JClass -> JFieldID -> JObject -> JNI ()
setStaticObjectField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticObjectField} p)
  mkSetStaticObjectField f e cls fid v

-- void SetStaticBooleanField(JNIEnv *env, jclass clazz, jfieldID fieldID, jboolean value);
foreign import ccall "dynamic" mkSetStaticBooleanField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JBoolean -> IO()) -> JNIEnv -> JClass -> JFieldID -> JBoolean -> IO ()
setStaticBooleanField :: JClass -> JFieldID -> JBoolean -> JNI ()
setStaticBooleanField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticBooleanField} p)
  mkSetStaticBooleanField f e cls fid v

-- void SetStaticByteField(JNIEnv *env, jclass clazz, jfieldID fieldID, jbyte value);
foreign import ccall "dynamic" mkSetStaticByteField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JByte -> IO()) -> JNIEnv -> JClass -> JFieldID -> JByte -> IO ()
setStaticByteField :: JClass -> JFieldID -> JByte -> JNI ()
setStaticByteField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticByteField} p)
  mkSetStaticByteField f e cls fid v

-- void SetStaticCharField(JNIEnv *env, jclass clazz, jfieldID fieldID, jchar value);
foreign import ccall "dynamic" mkSetStaticCharField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JChar -> IO()) -> JNIEnv -> JClass -> JFieldID -> JChar -> IO ()
setStaticCharField :: JClass -> JFieldID -> JChar -> JNI ()
setStaticCharField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticCharField} p)
  mkSetStaticCharField f e cls fid v

-- void SetStaticShortField(JNIEnv *env, jclass clazz, jfieldID fieldID, jshort value);
foreign import ccall "dynamic" mkSetStaticShortField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JShort -> IO()) -> JNIEnv -> JClass -> JFieldID -> JShort -> IO ()
setStaticShortField :: JClass -> JFieldID -> JShort -> JNI ()
setStaticShortField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticShortField} p)
  mkSetStaticShortField f e cls fid v

-- void SetStaticIntField(JNIEnv *env, jclass clazz, jfieldID fieldID, jint value);
foreign import ccall "dynamic" mkSetStaticIntField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JInt -> IO()) -> JNIEnv -> JClass -> JFieldID -> JInt -> IO ()
setStaticIntField :: JClass -> JFieldID -> JInt -> JNI ()
setStaticIntField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticIntField} p)
  mkSetStaticIntField f e cls fid v

-- void SetStaticLongField(JNIEnv *env, jclass clazz, jfieldID fieldID, jlong value);
foreign import ccall "dynamic" mkSetStaticLongField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JLong -> IO()) -> JNIEnv -> JClass -> JFieldID -> JLong -> IO ()
setStaticLongField :: JClass -> JFieldID -> JLong -> JNI ()
setStaticLongField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticLongField} p)
  mkSetStaticLongField f e cls fid v

-- void SetStaticFloatField(JNIEnv *env, jclass clazz, jfieldID fieldID, jfloat value);
foreign import ccall "dynamic" mkSetStaticFloatField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JFloat -> IO()) -> JNIEnv -> JClass -> JFieldID -> JFloat -> IO ()
setStaticFloatField :: JClass -> JFieldID -> JFloat -> JNI ()
setStaticFloatField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticFloatField} p)
  mkSetStaticFloatField f e cls fid v

-- void SetStaticDoubleField(JNIEnv *env, jclass clazz, jfieldID fieldID, jdouble value);
foreign import ccall "dynamic" mkSetStaticDoubleField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JDouble -> IO()) -> JNIEnv -> JClass -> JFieldID -> JDouble -> IO ()
setStaticDoubleField :: JClass -> JFieldID -> JDouble -> JNI ()
setStaticDoubleField cls fid v = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetStaticDoubleField} p)
  mkSetStaticDoubleField f e cls fid v

class SetStaticField a where setStaticField :: JClass -> JFieldID -> a -> JNI ()
instance SetStaticField JObject where setStaticField = setStaticObjectField
instance SetStaticField JBoolean where setStaticField = setStaticBooleanField
instance SetStaticField JByte where setStaticField = setStaticByteField
instance SetStaticField JChar where setStaticField = setStaticCharField
instance SetStaticField JShort where setStaticField = setStaticShortField
instance SetStaticField JInt where setStaticField = setStaticIntField
instance SetStaticField JLong where setStaticField = setStaticLongField
instance SetStaticField JFloat where setStaticField = setStaticFloatField
instance SetStaticField JDouble where setStaticField = setStaticDoubleField

-- Calling Static Methods

-- jmethodID GetStaticMethodID(JNIEnv *env, jclass clazz, const char *name, const char *sig);
foreign import ccall "dynamic" mkGetStaticMethodID ::
  FunPtr (JNIEnv -> JClass -> CString -> CString -> IO JMethodID) -> JNIEnv -> JClass -> CString -> CString -> IO JMethodID
getStaticMethodID :: JClass -> String -> String -> JNI JMethodID
getStaticMethodID cls name sig = withEnv $ \e p -> withCString name $ \cname -> withCString sig $ \csig -> do
  f <- (#{peek struct JNINativeInterface_, GetStaticMethodID} p)
  mkGetStaticMethodID f e cls cname csig

-- void CallStaticVoidMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticVoidMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO ()) -> JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO ()
callStaticVoidMethod :: JClass -> JMethodID -> [JValue] -> JNI ()
callStaticVoidMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticVoidMethodA} p)
  mkCallStaticVoidMethodA f e cls m argv

-- jobject CallStaticObjectMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticObjectMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JObject) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JObject
callStaticObjectMethod :: JClass -> JMethodID -> [JValue] -> JNI JObject
callStaticObjectMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticObjectMethodA} p)
  mkCallStaticObjectMethodA f e cls m argv

-- jboolean CallStaticBooleanMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticBooleanMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JBoolean) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JBoolean
callStaticBooleanMethod :: JClass -> JMethodID -> [JValue] -> JNI JBoolean
callStaticBooleanMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticBooleanMethodA} p)
  mkCallStaticBooleanMethodA f e cls m argv

-- jbyte CallStaticByteMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticByteMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JByte) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JByte
callStaticByteMethod :: JClass -> JMethodID -> [JValue] -> JNI JByte
callStaticByteMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticByteMethodA} p)
  mkCallStaticByteMethodA f e cls m argv

-- jchar CallStaticCharMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticCharMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JChar) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JChar
callStaticCharMethod :: JClass -> JMethodID -> [JValue] -> JNI JChar
callStaticCharMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticCharMethodA} p)
  mkCallStaticCharMethodA f e cls m argv

-- jshort CallStaticShortMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticShortMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JShort) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JShort
callStaticShortMethod :: JClass -> JMethodID -> [JValue] -> JNI JShort
callStaticShortMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticShortMethodA} p)
  mkCallStaticShortMethodA f e cls m argv

-- jint CallStaticIntMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticIntMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JInt) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JInt
callStaticIntMethod :: JClass -> JMethodID -> [JValue] -> JNI JInt
callStaticIntMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticIntMethodA} p)
  mkCallStaticIntMethodA f e cls m argv

-- jlong CallStaticLongMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticLongMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JLong) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JLong
callStaticLongMethod :: JClass -> JMethodID -> [JValue] -> JNI JLong
callStaticLongMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticLongMethodA} p)
  mkCallStaticLongMethodA f e cls m argv

-- jfloat CallStaticFloatMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticFloatMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JFloat) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JFloat
callStaticFloatMethod :: JClass -> JMethodID -> [JValue] -> JNI JFloat
callStaticFloatMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticFloatMethodA} p)
  mkCallStaticFloatMethodA f e cls m argv

-- jdouble CallStaticDoubleMethodA(JNIEnv *env, jclass clazz, jmethodID methodID, const jvalue *args);
foreign import ccall "dynamic" mkCallStaticDoubleMethodA ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JDouble) ->  JNIEnv -> JClass -> JMethodID -> Ptr JValue -> IO JDouble
callStaticDoubleMethod :: JClass -> JMethodID -> [JValue] -> JNI JDouble
callStaticDoubleMethod cls m args = withEnv $ \e p -> withArray args $ \argv -> do
  f <- (#{peek struct JNINativeInterface_, CallStaticDoubleMethodA} p)
  mkCallStaticDoubleMethodA f e cls m argv

class CallStaticMethod a where callStaticMethod :: JClass -> JMethodID -> [JValue] -> JNI a
instance CallStaticMethod () where callStaticMethod = callStaticVoidMethod
instance CallStaticMethod JObject where callStaticMethod = callStaticObjectMethod
instance CallStaticMethod JBoolean where callStaticMethod = callStaticBooleanMethod
instance CallStaticMethod JByte where callStaticMethod = callStaticByteMethod
instance CallStaticMethod JChar where callStaticMethod = callStaticCharMethod
instance CallStaticMethod JShort where callStaticMethod = callStaticShortMethod
instance CallStaticMethod JInt where callStaticMethod = callStaticIntMethod
instance CallStaticMethod JLong where callStaticMethod = callStaticLongMethod
instance CallStaticMethod JFloat where callStaticMethod = callStaticFloatMethod
instance CallStaticMethod JDouble where callStaticMethod = callStaticDoubleMethod

-- String Operations

-- jstring NewString(JNIEnv *env, const jchar *unicodeChars, jsize len);
foreign import ccall "dynamic" mkNewString ::
  FunPtr (JNIEnv -> Ptr JChar -> JSize -> IO JString) -> JNIEnv -> Ptr JChar -> JSize -> IO JString
newString :: Text -> JNI JString
newString s =  withEnv $ \e p -> useAsPtr s $ \ps l -> do
  f <- (#{peek struct JNINativeInterface_, NewString} p)
  mkNewString f e ps $ fromIntegral l

fromJString :: JString -> JNI Text
fromJString s = do
  l <- getStringLength s
  p <- getStringCritical s
  r <- liftIO . fromPtr p . fromIntegral $ l
  releaseStringCritical s p
  return r

-- jsize GetStringLength(JNIEnv *env, jstring string);
foreign import ccall "dynamic" mkGetStringLength ::
  FunPtr (JNIEnv -> JString -> IO JSize) -> JNIEnv -> JString -> IO JSize
getStringLength :: JString -> JNI JSize
getStringLength s = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringLength} p)
  mkGetStringLength f e s

-- const jchar * GetStringChars(JNIEnv *env, jstring string, jboolean *isCopy);
foreign import ccall "dynamic" mkGetStringChars ::
  FunPtr (JNIEnv -> JString -> Ptr JBoolean -> IO (Ptr JChar)) -> JNIEnv -> JString -> Ptr JBoolean -> IO (Ptr JChar)
getStringChars :: JString -> JNI (Ptr JChar)
getStringChars s = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringChars} p)
  alloca $ \b -> mkGetStringChars f e s b

-- void ReleaseStringChars(JNIEnv *env, jstring string, const jchar *chars);
foreign import ccall "dynamic" mkReleaseStringChars ::
  FunPtr (JNIEnv -> JString -> Ptr JChar -> IO ()) -> JNIEnv -> JString -> Ptr JChar -> IO ()
releaseStringChars :: JString -> Ptr JChar -> JNI ()
releaseStringChars s cs = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseStringChars} p)
  mkReleaseStringChars f e s cs

-- jstring NewStringUTF(JNIEnv *env, const char *bytes);
foreign import ccall "dynamic" mkNewStringUTF ::
  FunPtr (JNIEnv -> Ptr Char -> IO JString) -> (JNIEnv -> Ptr Char -> IO JString)
newStringUTF :: Ptr Char -> JNI JString
newStringUTF cs = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewStringUTF} p)
  mkNewStringUTF f e cs

-- jsize GetStringUTFLength(JNIEnv *env, jstring string);
foreign import ccall "dynamic" mkGetStringUTFLength ::
  FunPtr (JNIEnv -> JString -> IO JSize) -> (JNIEnv -> JString -> IO JSize)
getStringUTFLength :: JString -> JNI JSize
getStringUTFLength str = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringUTFLength} p)
  mkGetStringUTFLength f e str

-- const char * GetStringUTFChars(JNIEnv *env, jstring string, jboolean *isCopy);
foreign import ccall "dynamic" mkGetStringUTFChars ::
  FunPtr (JNIEnv -> JString -> Ptr JBoolean -> IO (Ptr CChar)) -> (JNIEnv -> JString -> Ptr JBoolean -> IO (Ptr CChar))
getStringUTFChars :: JString -> JNI (Ptr CChar)
getStringUTFChars str = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringUTFChars} p)
  mkGetStringUTFChars f e str nullPtr

-- void ReleaseStringUTFChars(JNIEnv *env, jstring string, const char *utf);
foreign import ccall "dynamic" mkReleaseStringUTFChars ::
  FunPtr (JNIEnv -> JString -> Ptr CChar -> IO ()) -> (JNIEnv -> JString -> Ptr CChar -> IO ())
releaseStringUTFChars :: JString -> Ptr CChar -> JNI ()
releaseStringUTFChars str utf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseStringUTFChars} p)
  mkReleaseStringUTFChars f e str utf

-- void GetStringRegion(JNIEnv *env, jstring str, jsize start, jsize len, jchar *buf);
foreign import ccall "dynamic" mkGetStringRegion ::
  FunPtr (JNIEnv -> JString -> JSize -> JSize -> Ptr JChar -> IO ()) -> (JNIEnv -> JString -> JSize -> JSize -> Ptr JChar -> IO ())
getStringRegion :: JString -> JSize -> JSize -> Ptr JChar -> JNI ()
getStringRegion str start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringRegion} p)
  mkGetStringRegion f e str start len buf

-- void GetStringUTFRegion(JNIEnv *env, jstring str, jsize start, jsize len, char *buf);
foreign import ccall "dynamic" mkGetStringUTFRegion ::
  FunPtr (JNIEnv -> JString -> JSize -> JSize -> Ptr CChar -> IO ()) -> (JNIEnv -> JString -> JSize -> JSize -> Ptr CChar -> IO ())
getStringUTFRegion :: JString -> JSize -> JSize -> Ptr CChar -> JNI ()
getStringUTFRegion str start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringUTFRegion} p)
  mkGetStringUTFRegion f e str start len buf

-- const jchar * GetStringCritical(JNIEnv *env, jstring string, jboolean *isCopy);
foreign import ccall "dynamic" mkGetStringCritical ::
  FunPtr (JNIEnv -> JString -> Ptr JBoolean -> IO (Ptr JChar)) -> JNIEnv -> JString -> Ptr JBoolean -> IO (Ptr JChar)
getStringCritical :: JString -> JNI (Ptr JChar)
getStringCritical s = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetStringCritical} p)
  mkGetStringCritical f e s nullPtr

--void ReleaseStringCritical(JNIEnv *env, jstring string, const jchar *carray);
foreign import ccall "dynamic" mkReleaseStringCritical ::
  FunPtr (JNIEnv -> JString -> Ptr JChar -> IO ()) -> JNIEnv -> JString -> Ptr JChar -> IO ()
releaseStringCritical :: JString -> Ptr JChar -> JNI ()
releaseStringCritical s cs = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseStringCritical} p)
  mkReleaseStringCritical f e s cs

-- Array Operations

-- jsize GetArrayLength(JNIEnv *env, jarray array);
foreign import ccall "dynamic" mkGetArrayLength ::
  FunPtr (JNIEnv -> JArray -> IO JSize) -> (JNIEnv -> JArray -> IO JSize)
getArrayLength :: JArray -> JNI JSize
getArrayLength arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetArrayLength} p)
  mkGetArrayLength f e arr

-- jobjectArray NewObjectArray(JNIEnv *env, jsize length, jclass elementClass, jobject initialElement);
foreign import ccall "dynamic" mkNewObjectArray ::
  FunPtr (JNIEnv -> JSize -> JClass -> JObject -> IO JObjectArray) -> (JNIEnv -> JSize -> JClass -> JObject -> IO JObjectArray)
newObjectArray :: JSize -> JClass -> JObject -> JNI JObjectArray
newObjectArray sz cls o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewObjectArray} p)
  mkNewObjectArray f e sz cls o

-- jobject GetObjectArrayElement(JNIEnv *env, jobjectArray array, jsize index);
foreign import ccall "dynamic" mkGetObjectArrayElement ::
  FunPtr (JNIEnv -> JObjectArray -> JSize -> IO JObject) -> (JNIEnv -> JObjectArray -> JSize -> IO JObject)
getObjectArrayElement :: JObjectArray -> JSize -> JNI JObject
getObjectArrayElement arr idx = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetObjectArrayElement} p)
  mkGetObjectArrayElement f e arr idx

-- void SetObjectArrayElement(JNIEnv *env, jobjectArray array, jsize index, jobject value);
foreign import ccall "dynamic" mkSetObjectArrayElement ::
  FunPtr (JNIEnv -> JObjectArray -> JSize -> JObject -> IO ()) -> (JNIEnv -> JObjectArray -> JSize -> JObject -> IO ())
setObjectArrayElement :: JObjectArray -> JSize -> JObject -> JNI ()
setObjectArrayElement arr idx o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetObjectArrayElement} p)
  mkSetObjectArrayElement f e arr idx o

-- jbooleanArray NewBooleanArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewBooleanArray ::
  FunPtr (JNIEnv -> JSize -> IO JBooleanArray) -> (JNIEnv -> JSize -> IO JBooleanArray)
newBooleanArray :: JSize -> JNI JBooleanArray
newBooleanArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewBooleanArray} p)
  mkNewBooleanArray f e sz

-- jbyteArray NewByteArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewByteArray ::
  FunPtr (JNIEnv -> JSize -> IO JByteArray) -> (JNIEnv -> JSize -> IO JByteArray)
newByteArray :: JSize -> JNI JByteArray
newByteArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewByteArray} p)
  mkNewByteArray f e sz

-- jcharArray NewCharArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewCharArray ::
  FunPtr (JNIEnv -> JSize -> IO JCharArray) -> (JNIEnv -> JSize -> IO JCharArray)
newCharArray :: JSize -> JNI JCharArray
newCharArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewCharArray} p)
  mkNewCharArray f e sz

-- jshortArray NewShortArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewShortArray ::
  FunPtr (JNIEnv -> JSize -> IO JShortArray) -> (JNIEnv -> JSize -> IO JShortArray)
newShortArray :: JSize -> JNI JShortArray
newShortArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewShortArray} p)
  mkNewShortArray f e sz

-- jintArray NewIntArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewIntArray ::
  FunPtr (JNIEnv -> JSize -> IO JIntArray) -> (JNIEnv -> JSize -> IO JIntArray)
newIntArray :: JSize -> JNI JIntArray
newIntArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewIntArray} p)
  mkNewIntArray f e sz

-- jlongArray NewLongArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewLongArray ::
  FunPtr (JNIEnv -> JSize -> IO JLongArray) -> (JNIEnv -> JSize -> IO JLongArray)
newLongArray :: JSize -> JNI JLongArray
newLongArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewLongArray} p)
  mkNewLongArray f e sz

-- jfloatArray NewFloatArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewFloatArray ::
  FunPtr (JNIEnv -> JSize -> IO JFloatArray) -> (JNIEnv -> JSize -> IO JFloatArray)
newFloatArray :: JSize -> JNI JFloatArray
newFloatArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewFloatArray} p)
  mkNewFloatArray f e sz

-- jdoubleArray NewDoubleArray(JNIEnv *env, jsize length);
foreign import ccall "dynamic" mkNewDoubleArray ::
  FunPtr (JNIEnv -> JSize -> IO JDoubleArray) -> (JNIEnv -> JSize -> IO JDoubleArray)
newDoubleArray :: JSize -> JNI JDoubleArray
newDoubleArray sz = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewDoubleArray} p)
  mkNewDoubleArray f e sz

-- jboolean* GetBooleanArrayElements(JNIEnv *env, jbooleanArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetBooleanArrayElements ::
  FunPtr (JNIEnv -> JBooleanArray -> Ptr JBoolean -> IO (Ptr JBoolean)) -> (JNIEnv -> JBooleanArray -> Ptr JBoolean -> IO (Ptr JBoolean))
getBooleanArrayElements :: JBooleanArray -> JNI (Ptr JBoolean)
getBooleanArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetBooleanArrayElements} p)
  mkGetBooleanArrayElements f e arr nullPtr

-- jbyte* GetByteArrayElements(JNIEnv *env, jbyteArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetByteArrayElements ::
  FunPtr (JNIEnv -> JByteArray -> Ptr JBoolean -> IO (Ptr JByte)) -> (JNIEnv -> JByteArray -> Ptr JBoolean -> IO (Ptr JByte))
getByteArrayElements :: JByteArray -> JNI (Ptr JByte)
getByteArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetByteArrayElements} p)
  mkGetByteArrayElements f e arr nullPtr

-- jchar* GetCharArrayElements(JNIEnv *env, jcharArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetCharArrayElements ::
  FunPtr (JNIEnv -> JCharArray -> Ptr JBoolean -> IO (Ptr JChar)) -> (JNIEnv -> JCharArray -> Ptr JBoolean -> IO (Ptr JChar))
getCharArrayElements :: JCharArray -> JNI (Ptr JChar)
getCharArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetCharArrayElements} p)
  mkGetCharArrayElements f e arr nullPtr

-- jshort* GetShortArrayElements(JNIEnv *env, jshortArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetShortArrayElements ::
  FunPtr (JNIEnv -> JShortArray -> Ptr JBoolean -> IO (Ptr JShort)) -> (JNIEnv -> JShortArray -> Ptr JBoolean -> IO (Ptr JShort))
getShortArrayElements :: JShortArray -> JNI (Ptr JShort)
getShortArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetShortArrayElements} p)
  mkGetShortArrayElements f e arr nullPtr

-- jint* GetIntArrayElements(JNIEnv *env, jintArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetIntArrayElements ::
  FunPtr (JNIEnv -> JIntArray -> Ptr JBoolean -> IO (Ptr JInt)) -> (JNIEnv -> JIntArray -> Ptr JBoolean -> IO (Ptr JInt))
getIntArrayElements :: JIntArray -> JNI (Ptr JInt)
getIntArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetIntArrayElements} p)
  mkGetIntArrayElements f e arr nullPtr

-- jlong* GetLongArrayElements(JNIEnv *env, jlongArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetLongArrayElements ::
  FunPtr (JNIEnv -> JLongArray -> Ptr JBoolean -> IO (Ptr JLong)) -> (JNIEnv -> JLongArray -> Ptr JBoolean -> IO (Ptr JLong))
getLongArrayElements :: JLongArray -> JNI (Ptr JLong)
getLongArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetLongArrayElements} p)
  mkGetLongArrayElements f e arr nullPtr

-- jfloat* GetFloatArrayElements(JNIEnv *env, jfloatArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetFloatArrayElements ::
  FunPtr (JNIEnv -> JFloatArray -> Ptr JBoolean -> IO (Ptr JFloat)) -> (JNIEnv -> JFloatArray -> Ptr JBoolean -> IO (Ptr JFloat))
getFloatArrayElements :: JFloatArray -> JNI (Ptr JFloat)
getFloatArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetFloatArrayElements} p)
  mkGetFloatArrayElements f e arr nullPtr

-- jdouble* GetDoubleArrayElements(JNIEnv *env, jdoubleArray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetDoubleArrayElements ::
  FunPtr (JNIEnv -> JDoubleArray -> Ptr JBoolean -> IO (Ptr JDouble)) -> (JNIEnv -> JDoubleArray -> Ptr JBoolean -> IO (Ptr JDouble))
getDoubleArrayElements :: JDoubleArray -> JNI (Ptr JDouble)
getDoubleArrayElements arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetDoubleArrayElements} p)
  mkGetDoubleArrayElements f e arr nullPtr

-- void ReleaseBooleanArrayElements(JNIEnv *env, jbooleanArray array, jboolean *elems, jint mode);
foreign import ccall "dynamic" mkReleaseBooleanArrayElements ::
  FunPtr (JNIEnv -> JBooleanArray -> Ptr JBoolean -> JInt -> IO ()) -> (JNIEnv -> JBooleanArray -> Ptr JBoolean -> JInt -> IO ())
releaseBooleanArrayElements :: JBooleanArray -> Ptr JBoolean -> JInt -> JNI ()
releaseBooleanArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseBooleanArrayElements} p)
  mkReleaseBooleanArrayElements f e arr elems mode

-- void ReleaseByteArrayElements(JNIEnv *env, jbyteArray array, jbyte *elems, jint mode);
foreign import ccall "dynamic" mkReleaseByteArrayElements ::
  FunPtr (JNIEnv -> JByteArray -> Ptr JByte -> JInt -> IO ()) -> (JNIEnv -> JByteArray -> Ptr JByte -> JInt -> IO ())
releaseByteArrayElements :: JByteArray -> Ptr JByte -> JInt -> JNI ()
releaseByteArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseByteArrayElements} p)
  mkReleaseByteArrayElements f e arr elems mode

-- void ReleaseCharArrayElements(JNIEnv *env, jcharArray array, jchar *elems, jint mode);
foreign import ccall "dynamic" mkReleaseCharArrayElements ::
  FunPtr (JNIEnv -> JCharArray -> Ptr JChar -> JInt -> IO ()) -> (JNIEnv -> JCharArray -> Ptr JChar -> JInt -> IO ())
releaseCharArrayElements :: JCharArray -> Ptr JChar -> JInt -> JNI ()
releaseCharArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseCharArrayElements} p)
  mkReleaseCharArrayElements f e arr elems mode

-- void ReleaseShortArrayElements(JNIEnv *env, jshortArray array, jshort *elems, jint mode);
foreign import ccall "dynamic" mkReleaseShortArrayElements ::
  FunPtr (JNIEnv -> JShortArray -> Ptr JShort -> JInt -> IO ()) -> (JNIEnv -> JShortArray -> Ptr JShort -> JInt -> IO ())
releaseShortArrayElements :: JShortArray -> Ptr JShort -> JInt -> JNI ()
releaseShortArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseShortArrayElements} p)
  mkReleaseShortArrayElements f e arr elems mode

-- void ReleaseIntArrayElements(JNIEnv *env, jintArray array, jint *elems, jint mode);
foreign import ccall "dynamic" mkReleaseIntArrayElements ::
  FunPtr (JNIEnv -> JIntArray -> Ptr JInt -> JInt -> IO ()) -> (JNIEnv -> JIntArray -> Ptr JInt -> JInt -> IO ())
releaseIntArrayElements :: JIntArray -> Ptr JInt -> JInt -> JNI ()
releaseIntArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseIntArrayElements} p)
  mkReleaseIntArrayElements f e arr elems mode

-- void ReleaseLongArrayElements(JNIEnv *env, jlongArray array, jlong *elems, jint mode);
foreign import ccall "dynamic" mkReleaseLongArrayElements ::
  FunPtr (JNIEnv -> JLongArray -> Ptr JLong -> JInt -> IO ()) -> (JNIEnv -> JLongArray -> Ptr JLong -> JInt -> IO ())
releaseLongArrayElements :: JLongArray -> Ptr JLong -> JInt -> JNI ()
releaseLongArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseLongArrayElements} p)
  mkReleaseLongArrayElements f e arr elems mode

-- void ReleaseFloatArrayElements(JNIEnv *env, jfloatArray array, jfloat *elems, jint mode);
foreign import ccall "dynamic" mkReleaseFloatArrayElements ::
  FunPtr (JNIEnv -> JFloatArray -> Ptr JFloat -> JInt -> IO ()) -> (JNIEnv -> JFloatArray -> Ptr JFloat -> JInt -> IO ())
releaseFloatArrayElements :: JFloatArray -> Ptr JFloat -> JInt -> JNI ()
releaseFloatArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseFloatArrayElements} p)
  mkReleaseFloatArrayElements f e arr elems mode

-- void ReleaseDoubleArrayElements(JNIEnv *env, jdoubleArray array, jdouble *elems, jint mode);
foreign import ccall "dynamic" mkReleaseDoubleArrayElements ::
  FunPtr (JNIEnv -> JDoubleArray -> Ptr JDouble -> JInt -> IO ()) -> (JNIEnv -> JDoubleArray -> Ptr JDouble -> JInt -> IO ())
releaseDoubleArrayElements :: JDoubleArray -> Ptr JDouble -> JInt -> JNI ()
releaseDoubleArrayElements arr elems mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleaseDoubleArrayElements} p)
  mkReleaseDoubleArrayElements f e arr elems mode

-- void GetBooleanArrayRegion(JNIEnv *env, jbooleanArray array, jsize start, jsize len, jboolean *buf);
foreign import ccall "dynamic" mkGetBooleanArrayRegion ::
  FunPtr (JNIEnv -> JBooleanArray -> JSize -> JSize -> Ptr JBoolean -> IO ()) -> (JNIEnv -> JBooleanArray -> JSize -> JSize -> Ptr JBoolean -> IO ())
getBooleanArrayRegion :: JBooleanArray -> JSize -> JSize -> Ptr JBoolean -> JNI ()
getBooleanArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetBooleanArrayRegion} p)
  mkGetBooleanArrayRegion f e arr start len buf

-- void GetByteArrayRegion(JNIEnv *env, jbyteArray array, jsize start, jsize len, jbyte *buf);
foreign import ccall "dynamic" mkGetByteArrayRegion ::
  FunPtr (JNIEnv -> JByteArray -> JSize -> JSize -> Ptr JByte -> IO ()) -> (JNIEnv -> JByteArray -> JSize -> JSize -> Ptr JByte -> IO ())
getByteArrayRegion :: JByteArray -> JSize -> JSize -> Ptr JByte -> JNI ()
getByteArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetByteArrayRegion} p)
  mkGetByteArrayRegion f e arr start len buf

-- void GetCharArrayRegion(JNIEnv *env, jcharArray array, jsize start, jsize len, jchar *buf);
foreign import ccall "dynamic" mkGetCharArrayRegion ::
  FunPtr (JNIEnv -> JCharArray -> JSize -> JSize -> Ptr JChar -> IO ()) -> (JNIEnv -> JCharArray -> JSize -> JSize -> Ptr JChar -> IO ())
getCharArrayRegion :: JCharArray -> JSize -> JSize -> Ptr JChar -> JNI ()
getCharArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetCharArrayRegion} p)
  mkGetCharArrayRegion f e arr start len buf

-- void GetShortArrayRegion(JNIEnv *env, jshortArray array, jsize start, jsize len, jhort *buf);
foreign import ccall "dynamic" mkGetShortArrayRegion ::
  FunPtr (JNIEnv -> JShortArray -> JSize -> JSize -> Ptr JShort -> IO ()) -> (JNIEnv -> JShortArray -> JSize -> JSize -> Ptr JShort -> IO ())
getShortArrayRegion :: JShortArray -> JSize -> JSize -> Ptr JShort -> JNI ()
getShortArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetShortArrayRegion} p)
  mkGetShortArrayRegion f e arr start len buf

-- void GetIntArrayRegion(JNIEnv *env, jintArray array, jsize start, jsize len, jint *buf);
foreign import ccall "dynamic" mkGetIntArrayRegion ::
  FunPtr (JNIEnv -> JIntArray -> JSize -> JSize -> Ptr JInt -> IO ()) -> (JNIEnv -> JIntArray -> JSize -> JSize -> Ptr JInt -> IO ())
getIntArrayRegion :: JIntArray -> JSize -> JSize -> Ptr JInt -> JNI ()
getIntArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetIntArrayRegion} p)
  mkGetIntArrayRegion f e arr start len buf

-- void GetLongArrayRegion(JNIEnv *env, jlongArray array, jsize start, jsize len, jlong *buf);
foreign import ccall "dynamic" mkGetLongArrayRegion ::
  FunPtr (JNIEnv -> JLongArray -> JSize -> JSize -> Ptr JLong -> IO ()) -> (JNIEnv -> JLongArray -> JSize -> JSize -> Ptr JLong -> IO ())
getLongArrayRegion :: JLongArray -> JSize -> JSize -> Ptr JLong -> JNI ()
getLongArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetLongArrayRegion} p)
  mkGetLongArrayRegion f e arr start len buf

-- void GetFloatArrayRegion(JNIEnv *env, jfloatArray array, jsize start, jsize len, jloat *buf);
foreign import ccall "dynamic" mkGetFloatArrayRegion ::
  FunPtr (JNIEnv -> JFloatArray -> JSize -> JSize -> Ptr JFloat -> IO ()) -> (JNIEnv -> JFloatArray -> JSize -> JSize -> Ptr JFloat -> IO ())
getFloatArrayRegion :: JFloatArray -> JSize -> JSize -> Ptr JFloat -> JNI ()
getFloatArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetFloatArrayRegion} p)
  mkGetFloatArrayRegion f e arr start len buf

-- void GetDoubleArrayRegion(JNIEnv *env, jdoubleArray array, jsize start, jsize len, jdouble *buf);
foreign import ccall "dynamic" mkGetDoubleArrayRegion ::
  FunPtr (JNIEnv -> JDoubleArray -> JSize -> JSize -> Ptr JDouble -> IO ()) -> (JNIEnv -> JDoubleArray -> JSize -> JSize -> Ptr JDouble -> IO ())
getDoubleArrayRegion :: JDoubleArray -> JSize -> JSize -> Ptr JDouble -> JNI ()
getDoubleArrayRegion arr start len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetDoubleArrayRegion} p)
  mkGetDoubleArrayRegion f e arr start len buf

-- void SetBooleanArrayRegion(JNIEnv *env, jbooleanArray array, jsize start, jsize len, const jboolean *buf);
foreign import ccall "dynamic" mkSetBooleanArrayRegion ::
  FunPtr (JNIEnv -> JBooleanArray -> JSize -> JSize -> Ptr JBoolean -> IO ()) -> (JNIEnv -> JBooleanArray -> JSize -> JSize -> Ptr JBoolean -> IO ())
setBooleanArrayRegion :: JBooleanArray -> JSize -> JSize -> Ptr JBoolean -> JNI ()
setBooleanArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetBooleanArrayRegion} p)
  mkSetBooleanArrayRegion f e arr sz len buf

-- void SetByteArrayRegion(JNIEnv *env, jbyteArray array, jsize start, jsize len, const jbyte *buf);
foreign import ccall "dynamic" mkSetByteArrayRegion ::
  FunPtr (JNIEnv -> JByteArray -> JSize -> JSize -> Ptr JByte -> IO ()) -> (JNIEnv -> JByteArray -> JSize -> JSize -> Ptr JByte -> IO ())
setByteArrayRegion :: JByteArray -> JSize -> JSize -> Ptr JByte -> JNI ()
setByteArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetByteArrayRegion} p)
  mkSetByteArrayRegion f e arr sz len buf

-- void SetCharArrayRegion(JNIEnv *env, jcharArray array, jsize start, jsize len, const jchar *buf);
foreign import ccall "dynamic" mkSetCharArrayRegion ::
  FunPtr (JNIEnv -> JCharArray -> JSize -> JSize -> Ptr JChar -> IO ()) -> (JNIEnv -> JCharArray -> JSize -> JSize -> Ptr JChar -> IO ())
setCharArrayRegion :: JCharArray -> JSize -> JSize -> Ptr JChar -> JNI ()
setCharArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetCharArrayRegion} p)
  mkSetCharArrayRegion f e arr sz len buf

-- void SetShortArrayRegion(JNIEnv *env, jshortArray array, jsize start, jsize len, const jshort *buf);
foreign import ccall "dynamic" mkSetShortArrayRegion ::
  FunPtr (JNIEnv -> JShortArray -> JSize -> JSize -> Ptr JShort -> IO ()) -> (JNIEnv -> JShortArray -> JSize -> JSize -> Ptr JShort -> IO ())
setShortArrayRegion :: JShortArray -> JSize -> JSize -> Ptr JShort -> JNI ()
setShortArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetShortArrayRegion} p)
  mkSetShortArrayRegion f e arr sz len buf

-- void SetIntArrayRegion(JNIEnv *env, jintArray array, jsize start, jsize len, const jint *buf);
foreign import ccall "dynamic" mkSetIntArrayRegion ::
  FunPtr (JNIEnv -> JIntArray -> JSize -> JSize -> Ptr JInt -> IO ()) -> (JNIEnv -> JIntArray -> JSize -> JSize -> Ptr JInt -> IO ())
setIntArrayRegion :: JIntArray -> JSize -> JSize -> Ptr JInt -> JNI ()
setIntArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetIntArrayRegion} p)
  mkSetIntArrayRegion f e arr sz len buf

-- void SetLongArrayRegion(JNIEnv *env, jlongArray array, jsize start, jsize len, const jlong *buf);
foreign import ccall "dynamic" mkSetLongArrayRegion ::
  FunPtr (JNIEnv -> JLongArray -> JSize -> JSize -> Ptr JLong -> IO ()) -> (JNIEnv -> JLongArray -> JSize -> JSize -> Ptr JLong -> IO ())
setLongArrayRegion :: JLongArray -> JSize -> JSize -> Ptr JLong -> JNI ()
setLongArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetLongArrayRegion} p)
  mkSetLongArrayRegion f e arr sz len buf

-- void SetFloatArrayRegion(JNIEnv *env, jfloatArray array, jsize start, jsize len, const jfloat *buf);
foreign import ccall "dynamic" mkSetFloatArrayRegion ::
  FunPtr (JNIEnv -> JFloatArray -> JSize -> JSize -> Ptr JFloat -> IO ()) -> (JNIEnv -> JFloatArray -> JSize -> JSize -> Ptr JFloat -> IO ())
setFloatArrayRegion :: JFloatArray -> JSize -> JSize -> Ptr JFloat -> JNI ()
setFloatArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetFloatArrayRegion} p)
  mkSetFloatArrayRegion f e arr sz len buf

-- void SetDoubleArrayRegion(JNIEnv *env, jdoubleArray array, jsize start, jsize len, const jdouble *buf);
foreign import ccall "dynamic" mkSetDoubleArrayRegion ::
  FunPtr (JNIEnv -> JDoubleArray -> JSize -> JSize -> Ptr JDouble -> IO ()) -> (JNIEnv -> JDoubleArray -> JSize -> JSize -> Ptr JDouble -> IO ())
setDoubleArrayRegion :: JDoubleArray -> JSize -> JSize -> Ptr JDouble -> JNI ()
setDoubleArrayRegion arr sz len buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, SetDoubleArrayRegion} p)
  mkSetDoubleArrayRegion f e arr sz len buf

--void * GetPrimitiveArrayCritical(JNIEnv *env, jarray array, jboolean *isCopy);
foreign import ccall "dynamic" mkGetPrimitiveArrayCritical ::
  FunPtr (JNIEnv -> JArray -> Ptr JBoolean -> IO (Ptr ())) ->  (JNIEnv -> JArray -> Ptr JBoolean -> IO (Ptr ()))
getPrimitiveArrayCritical :: JArray -> JNI (Ptr ())
getPrimitiveArrayCritical arr = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetPrimitiveArrayCritical} p)
  mkGetPrimitiveArrayCritical f e arr nullPtr

--void ReleasePrimitiveArrayCritical(JNIEnv *env, jarray array, void *carray, jint mode);
foreign import ccall "dynamic" mkReleasePrimitiveArrayCritical ::
  FunPtr (JNIEnv -> JArray -> Ptr () -> JInt -> IO ()) ->  (JNIEnv -> JArray -> Ptr () -> JInt -> IO ())
releasePrimitiveArrayCritical :: JArray -> Ptr () -> JInt -> JNI ()
releasePrimitiveArrayCritical arr parr mode = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ReleasePrimitiveArrayCritical} p)
  mkReleasePrimitiveArrayCritical f e arr parr mode

-- Registering Native Methods

type NativeMethodPtr = FunPtr ()

data NativeMethod = NativeMethod {
  nativeMethodName :: String,
  nativeMethodSignature :: String,
  nativeMethodFunPtr :: NativeMethodPtr
  }

toNativeMethodPtr :: FunPtr a -> NativeMethodPtr
toNativeMethodPtr = castFunPtr

data NativeMethod_ = NativeMethod_ CString CString NativeMethodPtr

instance Storable NativeMethod_ where
  sizeOf _ = #{size JNINativeMethod}
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    NativeMethod_
    <$> #{peek JNINativeMethod, name} p
    <*> #{peek JNINativeMethod, signature} p
    <*> #{peek JNINativeMethod, fnPtr} p
  poke p (NativeMethod_ name sig f) = do
    #{poke JNINativeMethod, name} p name
    #{poke JNINativeMethod, signature} p sig
    #{poke JNINativeMethod, fnPtr} p f

-- jint RegisterNatives(JNIEnv *env, jclass clazz, const JNINativeMethod *methods, jint nMethods);
foreign import ccall "dynamic" mkRegisterNatives ::
  FunPtr (JNIEnv -> JClass -> Ptr NativeMethod_ -> JInt -> IO JInt) -> (JNIEnv -> JClass -> Ptr NativeMethod_ -> JInt -> IO JInt)
registerNatives :: JClass -> [NativeMethod] -> JNI JNIError
registerNatives cls ms = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, RegisterNatives} p)
  ms_ <- mapM toC ms
  r <- withArray ms_ $ \cms -> do
    mkRegisterNatives f e cls cms (fromIntegral $ length ms)
  mapM_ freeC ms_
  return $ toJNIError r
  where
    toC (NativeMethod n s f) = NativeMethod_ <$> newCString n <*> newCString s <*> return f
    freeC (NativeMethod_ n s _f) = free n >> free s

-- jint UnregisterNatives(JNIEnv *env, jclass clazz);
foreign import ccall "dynamic" mkUnregisterNatives ::
  FunPtr (JNIEnv -> JClass -> IO JInt) -> (JNIEnv -> JClass -> IO JInt)
unregisterNatives :: JClass -> JNI JNIError
unregisterNatives cls = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, UnregisterNatives} p)
  mkUnregisterNatives f e cls >>= return . toJNIError

-- Monitor Operations

-- jint MonitorEnter(JNIEnv *env, jobject obj);
foreign import ccall "dynamic" mkMonitorEnter ::
  FunPtr (JNIEnv -> JObject -> IO JInt) ->  (JNIEnv -> JObject -> IO JInt)
monitorEnter :: JObject -> JNI JNIError
monitorEnter o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, MonitorEnter} p)
  mkMonitorEnter f e o >>= return . toJNIError

-- jint MonitorExit(JNIEnv *env, jobject obj);
foreign import ccall "dynamic" mkMonitorExit ::
  FunPtr (JNIEnv -> JObject -> IO JInt) -> (JNIEnv -> JObject -> IO JInt)
monitorExit :: JObject -> JNI JNIError
monitorExit o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, MonitorExit} p)
  mkMonitorExit f e o >>= return . toJNIError

-- NIO Support

-- jobject NewDirectByteBuffer(JNIEnv* env, void* address, jlong capacity);
foreign import ccall "dynamic" mkNewDirectByteBuffer ::
  FunPtr (JNIEnv -> Ptr () -> JLong -> IO JObject) -> (JNIEnv -> Ptr () -> JLong -> IO JObject)
newDirectByteBuffer :: Ptr () -> JLong -> JNI JObject
newDirectByteBuffer address capacity = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, NewDirectByteBuffer} p)
  mkNewDirectByteBuffer f e address capacity

-- void* GetDirectBufferAddress(JNIEnv* env, jobject buf);
foreign import ccall "dynamic" mkGetDirectBufferAddress ::
  FunPtr (JNIEnv -> JObject -> IO (Ptr ())) -> (JNIEnv -> JObject -> IO (Ptr ()))
getDirectBufferAddress :: JObject -> JNI (Ptr ())
getDirectBufferAddress buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetDirectBufferAddress} p)
  mkGetDirectBufferAddress f e buf

-- jlong GetDirectBufferCapacity(JNIEnv* env, jobject buf);
foreign import ccall "dynamic" mkGetDirectBufferCapacity ::
  FunPtr (JNIEnv -> JObject -> IO JLong) ->  (JNIEnv -> JObject -> IO JLong)
getDirectBufferCapacity :: JObject -> JNI JLong
getDirectBufferCapacity buf = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetDirectBufferCapacity} p)
  mkGetDirectBufferCapacity f e buf

-- Reflection Support

-- jmethodID FromReflectedMethod(JNIEnv *env, jobject method);
foreign import ccall "dynamic" mkFromReflectedMethod ::
  FunPtr (JNIEnv -> JObject -> IO JMethodID) -> (JNIEnv -> JObject -> IO JMethodID)
fromReflectedMethod :: JObject -> JNI JMethodID
fromReflectedMethod o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, FromReflectedMethod} p)
  mkFromReflectedMethod f e o

-- jfieldID FromReflectedField(JNIEnv *env, jobject field);
foreign import ccall "dynamic" mkFromReflectedField ::
  FunPtr (JNIEnv -> JObject -> IO JFieldID) -> (JNIEnv -> JObject -> IO JFieldID)
fromReflectedField :: JObject -> JNI JFieldID
fromReflectedField o = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, FromReflectedField} p)
  mkFromReflectedField f e o

-- jobject ToReflectedMethod(JNIEnv *env, jclass cls, jmethodID methodID, jboolean isStatic);
foreign import ccall "dynamic" mkToReflectedMethod ::
  FunPtr (JNIEnv -> JClass -> JMethodID -> JBoolean -> IO JObject) -> (JNIEnv -> JClass -> JMethodID -> JBoolean -> IO JObject)
toReflectedMethod :: JClass -> JMethodID -> JBoolean -> JNI JObject
toReflectedMethod cls meth isstatic = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ToReflectedMethod} p)
  mkToReflectedMethod f e cls meth isstatic

-- jobject ToReflectedField(JNIEnv *env, jclass cls, jfieldID fieldID, jboolean isStatic);
foreign import ccall "dynamic" mkToReflectedField ::
  FunPtr (JNIEnv -> JClass -> JFieldID -> JBoolean -> IO JObject) -> (JNIEnv -> JClass -> JFieldID -> JBoolean -> IO JObject)
toReflectedField :: JClass -> JFieldID -> JBoolean -> JNI JObject
toReflectedField cls fid isstatic = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, ToReflectedField} p)
  mkToReflectedField f e cls fid isstatic

-- Java VM Interface

-- jint GetJavaVM(JNIEnv *env, JavaVM **vm);
foreign import ccall "dynamic" mkGetJavaVM ::
  FunPtr (JNIEnv -> Ptr JavaVM -> IO JInt) -> JNIEnv -> Ptr JavaVM -> IO JInt
getJavaVM :: JNI (Either JNIError JavaVM)
getJavaVM = withEnv $ \e p -> do
  f <- (#{peek struct JNINativeInterface_, GetJavaVM} p)
  alloca $ \pvm -> do
    ret <- mkGetJavaVM f e pvm >>= return . toJNIError
    case ret of
      JNI_OK -> peek pvm >>= return . Right
      _ -> return $ Left ret

-- Invocation API Functions

-- jint JNI_GetDefaultJavaVMInitArgs(void *vm_args);
-- jint JNI_GetCreatedJavaVMs(JavaVM **vmBuf, jsize bufLen, jsize *nVMs);
-- jint JNI_CreateJavaVM(JavaVM **p_vm, void **p_env, void *vm_args);

-- jint DestroyJavaVM(JavaVM *vm);
foreign import ccall "dynamic" mkDestroyJavaVM ::
  FunPtr (JavaVM -> IO JInt) -> JavaVM -> IO JInt
destroyJavaVM :: JavaVM -> IO JNIError
destroyJavaVM vm = do
  p <- peek vm
  f <- (#{peek struct JNIInvokeInterface_, DestroyJavaVM} p)
  e <- mkDestroyJavaVM f vm
  return $ toJNIError e

-- jint AttachCurrentThread(JavaVM *vm, void **p_env, void *thr_args);
foreign import ccall "dynamic" mkAttachCurrentThread ::
  FunPtr (JavaVM -> Ptr JNIEnv -> Ptr () -> IO JInt) -> JavaVM -> Ptr JNIEnv -> Ptr () -> IO JInt
attachCurrentThread :: JavaVM -> IO (Either JNIError JNIEnv)
attachCurrentThread vm = do
  p <- peek vm
  f <- (#{peek struct JNIInvokeInterface_, AttachCurrentThread} p)
  alloca $ \penv -> do
    ret <- mkAttachCurrentThread f vm penv nullPtr
    let err = toJNIError ret
    case err of
      JNI_OK -> peek penv >>= return . Right
      _ -> return $ Left err

-- jint AttachCurrentThreadAsDaemon(JavaVM* vm, void** penv, void* args);
foreign import ccall "dynamic" mkAttachCurrentThreadAsDaemon ::
  FunPtr (JavaVM -> Ptr JNIEnv -> Ptr () -> IO JInt) -> JavaVM -> Ptr JNIEnv -> Ptr () -> IO JInt
attachCurrentThreadAsDaemon :: JavaVM -> IO (Either JNIError JNIEnv)
attachCurrentThreadAsDaemon vm = do
  p <- peek vm
  f <- (#{peek struct JNIInvokeInterface_, AttachCurrentThreadAsDaemon} p)
  alloca $ \penv -> do
    ret <- mkAttachCurrentThreadAsDaemon f vm penv nullPtr
    let err = toJNIError ret
    case err of
      JNI_OK -> peek penv >>= return . Right
      _ -> return $ Left err

-- jint DetachCurrentThread(JavaVM *vm);
foreign import ccall "dynamic" mkDetachCurrentThread ::
  FunPtr (JavaVM -> IO JInt) -> JavaVM -> IO JInt
detachCurrentThread :: JavaVM -> IO JNIError
detachCurrentThread vm = do
  p <- peek vm
  f <- (#{peek struct JNIInvokeInterface_, DetachCurrentThread} p)
  ret <- mkDetachCurrentThread f vm
  return $ toJNIError ret

-- jint GetEnv(JavaVM *vm, void **env, jint version);
foreign import ccall "dynamic" mkGetEnv ::
  FunPtr (JavaVM -> Ptr JNIEnv -> JInt -> IO JInt) -> JavaVM -> Ptr JNIEnv -> JInt -> IO JInt
getEnv :: JavaVM -> IO (Either JNIError JNIEnv)
getEnv vm = do
  p <- peek vm
  f <- (#{peek struct JNIInvokeInterface_, GetEnv} p)
  alloca $ \penv -> do
    ret <- mkGetEnv f vm penv #{const JNI_VERSION_1_6}
    let err = toJNIError ret
    case err of
      JNI_OK -> peek penv >>= return . Right
      _ -> return $ Left err
