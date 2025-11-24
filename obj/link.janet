(import ./utils :prefix "")

# XXX: a hack -- could reuse parts of a better peg
(def l/import-grammar
  ~(sequence :s* "("
             :s* "import"
             :s+
             # target item
             (choice (sequence `"`
                               (capture
                                 (some
                                   (if-not (set ` \t\r\n\0\f\v"`) 1)))
                               `"`)
                     (capture (some (if-not (set " \t\r\n\0\f\v)") 1))))
             # the rest
             (choice ")"
                     (sequence :s+
                               (any (if-not ")" 1))
                               ")"))))

(comment

  (peg/match l/import-grammar "(import ./pegs)")
  # =>
  @["./pegs"]

  (peg/match l/import-grammar `(import ./pegs :prefix "")`)
  # =>
  @["./pegs"]

  (peg/match l/import-grammar `(import "./pegs")`)
  # =>
  @["./pegs"]

  )

# create single file of source from a starting janet file by:
#
# 1. starting at in-path, read line by line recording the content in
#    out-path unless the line is an import form.
#
# 2. if an import form is encountered, don't record the line in
#    out-path, instead if the file it refers to has not been visited,
#    visit the file and continue recursively.
(defn l/link
  [in-path out-path]
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
        (when (in seen a-path) (break))
        #
        (put seen a-path true)
        (with [in-file (file/open a-path :r)]
          (loop [line :iterate (file/read in-file :line)]
            (if-let [[name-path] (peg/match l/import-grammar line)]
              (helper (string name-path ".janet"))
              (file/write out-file line)))))
      (helper file-path)
      (file/flush out-file))))

