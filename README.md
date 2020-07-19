# Bond

Bond is a spying and stubbing library, primarily intended for tests. This is a forked version from https://github.com/circleci/bond/ with support for
per-thread stubbing and spying. The project version of this is "<circle-ci-bond-base>.<forked-version>".

```clojure
[mourjo/bond "0.3.2.0.1.0"]
```

# Usage

```clojure

(ns test.foo
  (:require [bond.james :as bond :refer [with-spy with-local-spy]]
            [clojure.test :refer :all]))

(defn foo [x]
  (let [shaken (with-out-str (prn :martini))]
    [shaken]))

(defn bar [y]
  (foo y))

(deftest foo-is-called
  (with-spy [foo]
    (bar 2)
    (is (= 1 (-> foo bond/calls count)))))


;; local redefinition API:
(deftest foo-is-redefined-locally-only
  (let [futures [(future
                   (Thread/sleep 1000)
                   (with-local-spy [foo]
                     (= 1 (-> foo bond/local-calls count))))
                 (future
                   @(future
                      (Thread/sleep 1000)
                      (with-local-spy [foo]
                        (= 1 (-> foo bond/local-calls count)))))]]
    (with-spy [foo]
      (bar 2)
      (is (= 1 (-> foo bond/calls count))))
    (every? (fn [f] (true? @f)) futures)))

```

Bond provides two main macros, `with-spy` and `with-local-spy`. It takes a vector of defn vars (vars that resolve to fns). Each var will be redefined for the scope of the macro, wrapping the function to track arguments and call counts. At any point during the scope, you can call `(bond/calls f)`, where `f` is a spied fn. `calls` returns a seq of maps, one for each call to `f`b. Each map contains the keys `:args`, a seq of args the fn was called with, and one of `:return` or `:throw`. The only difference between `with-spy` and `with-local-spy` is that `with-local-spy` redefines the fn only for the current thread. Other threads are not affected. This behaviour is same for all `with-local*` functions, and must be used in conjuction with `local-calls` instead of `calls`.

Bond also provides `with-stub!` and `with-local-stub!`. It works the same as `with-spy`, but redefines the function to return `(constantly nil)` (default), while also spying on it. This is generally preferable to Clojure's built-in `with-redefs` macro since it will throw an exception if the mocked function is called with the wrong number of arguments. You can specify an arbitrary function instead of the default `(constantly nil)` by providing a `[fn-var replacement-fn]` vector in place of just the fn name:

```clojure
(ns test.foo
  (:require [bond.james :as bond :refer [with-stub!]]))

(defn foo [x] ...)

(defn bar [y] ...)

(deftest foo-is-called
  (with-stub! [[foo (fn [x] "foo")]
               [bar (fn [y] "bar")]]
    (is (= ["foo" "bar"] [(foo 1) (bar 2)]))))

(deftest consecutive-stubbing
  (with-stub! [[foo [(fn [x] "foo1")
                     (fn [x] "foo2")
                     (fn [x] "foo3")]]
               [bar (fn [y] "bar")]]
    (is (= ["foo1" "foo2" "foo3" "bar"] [(foo 1) (foo 1) (foo 1) (bar 2)]))))


(deftest foo-is-called-from-multiple-futures
  (let [futures [(future
                   (Thread/sleep 1000)
                   (bond/with-local-stub! [[foo (fn [x] "foo")]
                                           [bar (fn [y] "bar")]]
                     (= ["foo" "bar"] [(foo 1) (bar 2)])
                     (is (= 1 (-> foo bond/local-calls count)))))
                 (future
                   (Thread/sleep 1000)
                   (bond/with-local-stub! [[foo (fn [x] "foo 2")]
                                           [bar (fn [y] "bar 2")]]
                     (= ["foo 2" "bar 2"] [(foo 1) (bar 2)])
                     (is (= 1 (-> foo bond/local-calls count)))))]]
    (bond/with-stub! [[foo (fn [x] "foo 1")]
                      [bar (fn [y] "bar 2")]]
      (is (= ["foo 1" "bar 2"] [(foo 1) (bar 2)]))
      (is (= 1 (-> foo bond/calls count))))
    (is (every? true? (map deref futures)))))

```

Private functions can also be stubbed or spyed:

``` clojure
(ns test.foo)

(defn- foo [x] ...)
```

``` clojure
(ns test.bar
  (:require [bond.james :as bond :refer [with-stub!]]
            [test.foo :as foo]))

(deftest foo-is-called
  (with-local-stub! [[foo/foo (fn [x] "foo")]]
    (is (= "foo" (#'foo/foo 1)))
    (is (= [1] (-> #'foo/foo bond/local-calls first :args)))))
```

There is also a `with-stub` and `with-local-stub` macro which works like `with-stub!` and `with-local-stub!` but omits the argument check.

In addition to `with-spy*` and `with-stub*`, Bond also provides `with-spy-ns`/`with-local-spy-ns`
and `with-stub-ns` which can spy/stub every function in a namespace in one go:

```clojure
(ns test.foo
  (:require [bond.james :as bond]
            [clojure.test :refer (deftest is)]))

(defn foo [] :foo)

(defn bar [] :bar)

(deftest you-can-stub-entire-ns
  (is (= :foo (foo)))
  (is (= :bar (bar)))
  (bond/with-stub-ns [[foo (constantly :baz)]]
    (is (= :baz (foo)))
    (is (= :baz (bar)))))
```

There is an utility `with-dynamic-redefs` which can be used in place of `with-redefs` but the redefinition will only be seen by the current (Clojure) thread. This
can be used aside from the rest of the bond library as well.

# Notes

The local* functions work with Clojure's `binding` conveyance, so if the body of the redefinition goes beyond Clojure threads, like
say, a java.lang.Thread, then the binding conveyance will not work and the redefined definition will not be seen by the thread nor
will the counts to those functions from that thread be correct.

The local* functions only support redefinition/spying of functions for now. As a future todo, we plan to add support for vars along with functions.

# License

Distributed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
