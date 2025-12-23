(import ./common :as c)
(import ./jipper :as j)
(import ./utils :as u)

# create single file of source from appropriately modified set of
# files (see the code in prepare), beginning with a starting janet
# file by:
#
# 1. create a zipper from in-path's content, then traverse to the
#    right, recording the corresponding source code to out-path unless
#    the encountered zloc has a node representing an import form.
#
# 2. if an import form is encountered, record a commented version of
#    it in out-path, and if the file the import form refers to has not
#    been visited, visit the file and continue recursively.
(defn link
  [in-path out-path]
  (u/maybe-dump :call "link" :in-path in-path :out-path out-path)
  (def {:sep sep} (u/get-os-bits))
  # assumes paths are full paths...
  # XXX: could check if we had abspath?
  (def [dir-path file-path] (u/split-path in-path))
  # remember which files have already been "imported"
  (def seen @{})
  # for restoring the current working directory (cwd)
  (def old-dir (os/cwd))
  # need to operate relative to in-path's dir
  (os/cd dir-path)
  #
  (defer (os/cd old-dir)
    (with [out-file (file/open out-path :w)]
      (defn helper
        [a-path]
        (def [a-dir a-name] (u/split-path a-path))
        (def full-path (os/realpath (string (os/cwd) sep a-name)))
        (when (in seen full-path) (break))
        #
        (put seen full-path true)
        (var zloc
          (try (-> (slurp full-path)
                   j/par
                   j/zip-down)
            ([e] (errorf "failed to prepare zloc from: %s" full-path))))
        (while zloc
          (def cur-node (j/node zloc))
          (if (c/is-import? zloc)
            (let [i-tbl (c/analyze-import cur-node)
                  commented (-> zloc
                                (j/insert-child [:whitespace {} " "])
                                (j/insert-child [:symbol {} "comment"])
                                j/node
                                j/gen)]
              (file/write out-file commented "\n")
              (def i-path (get i-tbl :path))
              # parse import path
              (def [dir name] (u/split-path i-path))
              (def cur-dir (os/cwd))
              #
              (os/cd dir)
              (helper (string i-path ".janet"))
              (os/cd cur-dir))
            (file/write out-file (j/gen cur-node)))
          (set zloc (j/right zloc))))
      #
      (helper in-path)
      (file/flush out-file))))

