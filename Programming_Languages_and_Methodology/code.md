``` clojure
#(println "hello")
(defn hello [name](println "hello," name))
(hello "yvon")
(def a 1)
(println a)

(loop [x 10]
(when (> x 1)
(println x)
(recur (- x 2))))

(+ 3 4)
(* 2 7)
(* 2 (+ 3 4))
(list 1 2 3)
(def list_test(list 1 2 3))
(println list_test)
(println (first list_test))
(println (rest list_test))
(println (cons 0 list_test))

(def a (atom 10))
(while (pos? @a) (do (println @a) (swap! a dec)))

(println (class 12.56))

(import java.util.Date)
(println (Date.))

(def vector_test [:a :b :c :d])
(println (nth vector_test 3))
(println (concat vector_test [:e :f]))
(println (first vector_test))
(println (rest vector_test))
(println (last vector_test))

(defn size [v]
(if (empty? v)
0
(inc (size (rest v)))))
(println (size vector_test))

```


```
hello, yvon
1
10
8
6
4
2
(1 2 3)
1
(2 3)
(0 1 2 3)
10
9
8
7
6
5
4
3
2
1
java.lang.Double
#inst "2017-01-10T04:24:51.685-00:00"
:d
(:a :b :c :d :e :f)
:a
(:b :c :d)
:d
4
```










