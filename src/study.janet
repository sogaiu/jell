(import ./common :as c)
(import ./jipper :as j)
(import ./utils :as u)

(defn find-files-and-imports
  [in-path]
  # assumes paths are full paths...
  # XXX: could check if we had abspath?
  (def [dir-path file-path] (u/split-path in-path))
  # remember which files have already been "imported"
  (def seen @{})
  (def imports @{})
  # for restoring the current working directory (cwd)
  (def old-dir (os/cwd))
  # need to operate relative to in-path's dir
  (os/cd dir-path)
  #
  (defer (os/cd old-dir)
    (defn helper
      [a-path]
      (when (in seen a-path) (break))
      #
      (assertf (= :file (os/stat a-path :mode))
               "file does not exist or not a file: %s" a-path)
      (put seen a-path true)
      (def src (slurp a-path))
      (when (not (empty? src))
        (def tree (j/par src))
        (assertf tree "failed to parse: %s" a-path)
        (def zloc (j/zip-down tree))
        (assertf zloc "zip-down failed for tree for path: %s" a-path)
        (var cur-zloc zloc)
        (put imports a-path @[])
        (def import-paths (get imports a-path))
        (while (def i-zloc
                 (j/search-from cur-zloc |(match (j/node $) [:tuple]
                                            (when (c/is-import? $)
                                              $))))
          # warn about non-top-level imports
          (when (<= 2 (length (j/path i-zloc)))
            (def [_ {:bl bl} _] (j/node i-zloc))
            # issue might arise because there might be an expectation
            # of using the return value
            (eprintf "non-top-level import in %s at line: %d" a-path bl))
          #
          (set cur-zloc (j/df-next i-zloc))
          (def i-node (j/node i-zloc))
          (def i-stats (c/analyze-import i-node))
          (assertf (not= true (get i-stats :export))
                   "import with :export true in %s not supported: %n"
                   a-path (j/gen i-node))
          (def i-path (get i-stats :path))
          (assertf (string/has-prefix? "./" i-path)
                   "path should start with `./`, but found: %s" i-path)
          (assertf (= 1 (length (string/find-all "/" i-path)))
                   "only sibling files allowed: %s" i-path)
          (def j-file (os/realpath (string i-path ".janet")))
          (def prefix
            (cond
              (def as (get i-stats :as))
              (string as)
              #
              (def pfx (get i-stats :prefix))
              pfx
              # remove leading ./
              (string/slice i-path 2)))
          (put imports a-path
               (array/push import-paths [i-path j-file prefix]))
          (helper j-file))))
    #
    (helper (os/realpath file-path))
    #
    imports))

(defn study
  [start-path]
  (u/maybe-dump :call "study" :start-path start-path)
  (def files-and-imports (find-files-and-imports start-path))
  (u/maybe-dump :files-and-imports files-and-imports)
  #
  (def prefixes
    (reduce (fn [acc i-stats]
              (each [pth _ pfx] i-stats
                (if-let [o-pfx (get acc pth)]
                  (assertf (= pfx o-pfx)
                           "prefixes don't match: %s != %s" pfx o-pfx)
                  (put acc pth pfx)))
              acc)
            @{}
            (values files-and-imports)))
  #
  prefixes)

