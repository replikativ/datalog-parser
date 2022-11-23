(ns datalog.parser.pull
  (:require [datalog.parser.impl.util :as util
             #?(:cljs :refer-macros :clj :refer) [raise forv]])
  (:refer-clojure :rename {pos? core-pos?}))

#?(:clj (set! *warn-on-reflection* true))

(defn- pos? [n]
  (and (number? n) (core-pos? n)))

(defrecord PullSpec            [wildcard? attrs])
(defrecord PullAttrName        [attr])
(defrecord PullReverseAttrName [attr rattr])
(defrecord PullLimitExpr       [attr limit])
(defrecord PullDefaultExpr     [attr value])
(defrecord PullWildcard        [])
(defrecord PullRecursionLimit  [limit])
(defrecord PullMapSpecEntry    [attr porrl])
(defrecord PullAttrWithOpts    [attr opts])

(defprotocol IPullSpecComponent
  (-as-spec [this]))

(extend-protocol IPullSpecComponent
  PullAttrName
  (-as-spec [this]
    [(:attr this) {:attr (:attr this)}])
  PullReverseAttrName
  (-as-spec [this]
    [(:rattr this) {:attr (:attr this)}])
  PullLimitExpr
  (-as-spec [this]
    (-> (-as-spec (:attr this))
        (assoc-in [1 :limit] (:limit this))))
  PullDefaultExpr
  (-as-spec [this]
    (-> (-as-spec (:attr this))
        (assoc-in [1 :default] (:value this))))
  PullRecursionLimit
  (-as-spec [this]
    [:recursion (:limit this)])
  PullMapSpecEntry
  (-as-spec [this]
    (-> (-as-spec (:attr this))
        (update 1 conj (-as-spec (:porrl this)))))
  PullAttrWithOpts
  (-as-spec [this]
    (-> (-as-spec (:attr this))
        (update 1 merge (:opts this)))))

(defn- aggregate-specs [res part]
  (if (instance? PullWildcard part)
    (assoc  res :wildcard? true)
    (update res :attrs conj! (-as-spec part))))

(defrecord PullPattern [specs]
  IPullSpecComponent
  (-as-spec [this]
    (let [init (PullSpec. false (transient {}))
          spec (reduce aggregate-specs init specs)]
      [:subpattern (update spec :attrs persistent!)])))

(declare parse-pattern)

(def ^:private wildcard? #{'* :* "*"})

(defn- parse-wildcard [spec]
  (when (wildcard? spec)
    (PullWildcard.)))

(defn- parse-attr-name [spec]
  (when (or (keyword? spec) (string? spec))
    (if (util/reverse-ref? spec)
      (PullReverseAttrName. (util/reverse-ref spec) spec)
      (PullAttrName. spec))))

(def ^:private unlimited-recursion? #{'... "..."})

(defn- parse-recursion-limit [spec]
  (cond
    (unlimited-recursion? spec) (PullRecursionLimit. nil)
    (pos? spec)                 (PullRecursionLimit. spec)))

(def ^:private limit? #{'limit :limit "limit"})

(defn- parse-legacy-limit-expr [spec]
  (let [[limit-sym attr-name-spec pos-num] spec]
    (when (limit? limit-sym)
      (if-let [attr-name (and (or (nil? pos-num) (pos? pos-num))
                              (parse-attr-name attr-name-spec))]
        (PullLimitExpr. attr-name pos-num)
        (raise "Expected [\"limit\" attr-name (positive-number | nil)]"
               {:error :parser/pull, :fragment spec})))))

(def ^:private default? #{'default :default "default"})

(defn- parse-legacy-default-expr [spec]
  (let [[default-sym attr-name-spec default-val] spec]
    (when (default? default-sym)
      (if-let [attr-name (parse-attr-name attr-name-spec)]
        (PullDefaultExpr. attr-name default-val)
        (raise "Expected [\"default\" attr-name any-value]"
               {:error :parser/pull, :fragment spec})))))

(def ^:private opt? #{:as :limit :default})

(defn- parse-attr-with-opts [spec]
  (when (sequential? spec)
    (let [[attr-name-spec & opts-spec] spec]
      (if-some [attr-name (parse-attr-name attr-name-spec)]
        (if (even? (count opts-spec))
          (if-let [invalid-opt (first (drop-while opt? (take-nth 2 opts-spec)))]
            (raise (str "Invalid attribute option: " invalid-opt)
                   {:error :parser/pull, :fragment spec})
            (PullAttrWithOpts. attr-name (apply array-map opts-spec)))
          (raise "Option list must contain even number of elements"
                 {:error :parser/pull, :fragment spec}))
        (raise "Expected [attr-name attr-option+]"
               {:error :parser/pull, :fragment spec})))))

(defn- parse-legacy-attr-expr [spec]
  (when (and (sequential? spec)
             (= 3 (count spec)))
    (or (parse-legacy-limit-expr   spec)
        (parse-legacy-default-expr spec))))

(defn- parse-attr-expr [spec]
  (or (parse-legacy-attr-expr spec)
      (parse-attr-with-opts spec)))

(defn- parse-map-spec-entry [[k v]]
  (if-let [attr-name (or (parse-attr-name k)
                         (parse-attr-expr k))]
    (if-let [pattern-or-rec (or (parse-recursion-limit v)
                                (parse-pattern v))]
      (PullMapSpecEntry. attr-name pattern-or-rec)
      (raise "Expected (pattern | recursion-limit)"
             {:error :parser/pull, :fragment [k v]}))
    (raise "Expected (attr-name | attr-expr)"
           {:error :parser/pull, :fragment [k v]})))

(defn- parse-map-spec [spec]
  (when (map? spec)
    (assert (= 1 (count spec)) "Maps should contain exactly 1 entry")
    (parse-map-spec-entry (first spec))))

(defn- parse-attr-spec [spec]
  (or (parse-attr-name      spec)
      (parse-wildcard       spec)
      (parse-map-spec       spec)
      (parse-attr-expr      spec)
      (raise "Cannot parse attr-spec, expected: (attr-name | wildcard | map-spec | attr-expr)"
             {:error :parser/pull, :fragment spec})))

(defn- pattern-clause-type [clause]
  (cond
    (map? clause)      :map
    (wildcard? clause) :wildcard
    :else              :other))

(defn- expand-map-clause [clause]
  (forv [[k v] clause]
    {k v}))

(let [wildcarded? (comp not-empty :wildcard)]
  (defn- simplify-pattern-clauses [pattern]
    (let [groups (group-by pattern-clause-type pattern)
          base   (cond-> [] (wildcarded? groups) (conj '*))]
      (into base
        (concat
         (:other groups)
         (mapcat expand-map-clause (:map groups)))))))

(defn parse-pattern
  "Parse an EDN pull pattern into a tree of records using the following
grammar:

```
pattern             = [attr-spec+]
attr-spec           = attr-name | wildcard | map-spec | attr-expr
attr-name           = an edn keyword that names an attr
wildcard            = \"*\" or '*'
map-spec            = { ((attr-name | attr-expr) (pattern | recursion-limit))+ }
attr-expr           = attr-with-opts | legacy-attr-expr
attr-with-opts      = [attr-name attr-option+]
attr-option         = :as any-value | :limit (positive-number | nil) | :default any-value
recursion-limit     = positive-number | '...'
legacy-attr-expr    = legacy-limit-expr | legacy-default-expr
legacy-limit-expr   = [(\"limit\" | 'limit') attr-name (positive-number | nil)]
legacy-default-expr = [(\"default\" | 'default') attr-name any-value]
```"
  [pattern]
  (when (sequential? pattern)
    (->> pattern
         simplify-pattern-clauses
         (mapv parse-attr-spec)
         (PullPattern.))))

(defn pattern->spec
  "Convert a parsed tree of pull pattern records into a `PullSpec` instance,
a record type containing two keys:

* `:wildcard?` - a boolean indicating if the pattern contains a wildcard.
* `:attrs` - a map of attribute specifications.

The attribute specification map consists of keys which will become the keys
in the result map, and values which are themselves maps describing the
attribute:

* `:attr`       (required) - The attr name to pull; for reverse attributes
                             this will be the normalized attribute name.
* `:as`         (optional) - Alias, any
* `:limit`      (optional) - If present, specifies a custom limit for this
                             attribute; Either `nil`, indicating no limit,
                             or a positive integer.
* `:default`    (optional) - If present, specifies a default value for this
                             attribute
* `:recursion`  (optional) - If present, specifies a recursion limit for this
                             attribute; Either `nil`, indicating no limit, or
                             a positive integer.
* `:subpattern` (optional) - If present, specifies a sub `PullSpec` instance
                             to be applied to entities matched by this
                             attribute."
  [pattern]
  (second (-as-spec pattern)))

(defn parse-pull
  "Parse EDN pull `pattern` specification (see `parse-pattern`), and
convert the resulting tree into a `PullSpec` instance (see `pattern->spec`).
Throws an error if the supplied `pattern` cannot be parsed."
  [pattern]
  (or (some-> pattern parse-pattern pattern->spec)
      (raise "Cannot parse pull pattern, expected: [attr-spec+]"
             {:error :parser/pull, :fragment pattern})))
