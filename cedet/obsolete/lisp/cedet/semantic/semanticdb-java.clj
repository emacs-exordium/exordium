;;Clojure backend code for semantidb-java

(defn get-class-info [classname]
  "Return a list of members of CLASSNAME."
  (map (fn [x] (list (.getName x) (map  (fn [y] (.getName y )) (.getParameterTypes x)   )
                     (.getName (.getReturnType x))  ))
       (.getMethods (java.lang.Class/forName classname))))