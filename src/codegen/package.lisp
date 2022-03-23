(uiop:define-package #:coalton-impl/codegen
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:codegen-expression
   #:*emit-type-annotations*)
  (:export
   #:codegen-expression
   #:*emit-type-annotations*)

  (:import-from
   #:coalton-impl/codegen/compile-expression
   #:compile-expression)
  (:export
   #:compile-expression)

  (:import-from
   #:coalton-impl/codegen/program
   #:translation-unit
   #:make-translation-unit
   #:translation-unit-types
   #:translation-unit-definitions
   #:translation-unit-instances
   #:translation-unit-classes
   #:compile-translation-unit)
  (:export
   #:translation-unit
   #:make-translation-unit
   #:translation-unit-types
   #:translation-unit-definitions
   #:translation-unit-instances
   #:translation-unit-classes
   #:compile-translation-unit)

  (:import-from
   #:coalton-impl/codegen/function-entry
   #:a1 #:a2 #:a3 #:a4 #:a5 #:a6 #:a7 #:a8 #:a9
   #:f1 #:f2 #:f3 #:f4 #:f5 #:f6 #:f7 #:f8 #:f9)
  (:export
   #:a1 #:a2 #:a3 #:a4 #:a5 #:a6 #:a7 #:a8 #:a9
   #:f1 #:f2 #:f3 #:f4 #:f5 #:f6 #:f7 #:f8 #:f9))