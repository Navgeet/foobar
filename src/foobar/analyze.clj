(ns foobar.analyze
  (:require [clojure.tools.analyzer :as ana])
  (:import [clojure.lang Compiler$DefExpr Compiler$LocalBinding
            Compiler$BindingInit Compiler$LetExpr
            Compiler$LetFnExpr Compiler$StaticMethodExpr
            Compiler$InstanceMethodExpr Compiler$StaticFieldExpr
            Compiler$NewExpr Compiler$EmptyExpr
            Compiler$VectorExpr Compiler$MonitorEnterExpr
            Compiler$MonitorExitExpr Compiler$ThrowExpr
            Compiler$InvokeExpr Compiler$TheVarExpr
            Compiler$VarExpr Compiler$UnresolvedVarExpr
            Compiler$ObjExpr Compiler$NewInstanceMethod
            Compiler$FnMethod Compiler$FnExpr
            Compiler$NewInstanceExpr Compiler$MetaExpr
            Compiler$BodyExpr Compiler$ImportExpr
            Compiler$AssignExpr Compiler$TryExpr$CatchClause
            Compiler$TryExpr Compiler$C
            Compiler$LocalBindingExpr Compiler$RecurExpr
            Compiler$MapExpr Compiler$IfExpr
            Compiler$KeywordInvokeExpr Compiler$InstanceFieldExpr
            Compiler$InstanceOfExpr Compiler$CaseExpr
            Compiler$Expr Compiler$SetExpr
            Compiler$MethodParamExpr Compiler$KeywordExpr
            Compiler$ConstantExpr Compiler$NumberExpr
            Compiler$NilExpr Compiler$BooleanExpr
            Compiler$StringExpr Compiler$ObjMethod Compiler$Expr]))

(def special-types #{:number :keyword :constant :string :nil :boolean
                     :local-binding-expr})

(defn expr-obj-type [expr-obj ast]
  (if-let [op (special-types (:op ast))]
    op
    (let [form (.orig-form expr-obj)
          sym (first form)]
      (if (Compiler/isMacro sym)
        (resolve sym)
        (:op ast)))))

(defn ast-type [ast]
  (if-let [obj (some ast #{:LocalBinding-obj :BindingInit-obj
                           :ObjMethod-obj :CatchClause-obj})]
    (class obj)
    (if-let [expr-obj (:Expr-obj ast)]
      (expr-obj-type expr-obj ast)
      nil)))

(defmulti analyze-ast
  "Analyzes an ast recursively returning a map of attributes.
  Handles macros specially."
  ast-type)

(defmethod analyze-ast :default [ast]
  (if-let [obj-key (some ast #{:LocalBinding-obj :BindingInit-obj
                           :ObjMethod-obj :CatchClause-obj})]
    {obj-key (obj-key ast)}
    {:Expr-obj (:Expr-obj ast)}))

(defn analyze [expr]
  (cond (seq? expr) (map analyze-ast expr)
        (map? expr) (analyze-ast expr)
        :else expr))

(defmacro analyze-ast-literals [op]
  `(defmethod analyze-ast ~op [ast#]
     (let [{Expr-obj# :Expr-obj op# :op val# :val} ast#]
       {:Expr-obj Expr-obj# :op op# :val val#})))

(analyze-ast-literals :number)
(analyze-ast-literals :keyword)
(analyze-ast-literals :constant)
(analyze-ast-literals :string)
(analyze-ast-literals :nil)
(analyze-ast-literals :boolean)

(defmethod analyze-ast :local-binding-expr [ast]
  (let [{:keys [op tag local-binding Expr-obj]} ast]
    {:Expr-obj Expr-obj
     :op op
     :local-binding (analyze local-binding)
     :tag tag}))

(defmethod analyze-ast Compiler$LocalBinding [ast]
  (let [{:keys [op sym tag init LocalBinding-obj]} ast]
    {:LocalBinding-obj LocalBinding-obj
     :op op
     :sym sym
     :tag tag
     :init (analyze init)}))

(defmethod analyze-ast Compiler$BindingInit [ast]
  (let [{:keys [op local-binding init BindingInit-obj]} ast]
    {:BindingInit-obj BindingInit-obj
     :op op
     :local-binding (analyze local-binding)
     :init (analyze init)}))

(defmethod analyze-ast Compiler$ObjMethod [ast]
  (let [{:keys [op required-params rest-param ObjMethod-obj]
         {exprs :exprs} :body} ast]
    {:ObjMethod-obj ObjMethod-obj
     :op op
     :exprs (analyze exprs)
     :required-params (analyze required-params)
     :rest-param (analyze rest-param)}))

(defmethod analyze-ast #'clojure.core/fn [ast]
  (let [{:keys [methods variadic-method tag Expr-obj]} ast]
    {:Expr-obj Expr-obj
     :op 'clojure.core/fn
     :methods (analyze methods)
     :variadic-method variadic-method
     :tag tag}))

(defmethod analyze-ast #'clojure.core/defn [ast]
  (let [{:keys [var meta init Expr-obj]} ast]
    {:Expr-obj Expr-obj
     :op 'clojure.core/defn
     :var var
     :meta (analyze meta)
     :fn (analyze init)}))

(defmethod analyze-ast #'clojure.core/let [ast]
  (let [Expr-obj (:Expr-obj ast)
        ast (-> ast :fexpr :methods first :body :exprs first)
        {binding-inits :binding-inits {exprs :exprs} :body} ast]
    {:Expr-obj Expr-obj
     :op 'clojure.core/let
     :binding-inits (analyze binding-inits)
     :exprs (analyze exprs)}))
