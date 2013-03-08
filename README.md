foreign-jni
===========

Haskell bindings to the Java Native Interface. Ideal for writing Java
native methods in Haskell.

If the JNI headers are in your system include path just:

    cabal install

otherwise point cabal to your JAVA_HOME like this:

    cabal install --extra-include-dirs=$JAVA_HOME/include --extra-include-dirs=$JAVA_HOME/include/linu    cabal ins

Comments and patches are welcome.
