(defn maybe-dump
  [& args]
  (assertf (even? (length args))
           "expected even number or args: %n" args)
  #
  (when (os/getenv "VERBOSE")
    (each [name value] (partition 2 args)
      (cond
        (dictionary? value)
        (do
          (pp [name])
          (eachp [k v] value
            (printf "%n: %n" k v)))
        #
        (indexed? value)
        (do
          (pp [name])
          (each v value
            (pp v)))
        #
        (pp [name value])))
    (print)))

########################################################################

# XXX: more edge cases to identify?
#      * consecutive separator handling matches
#        posix more than python / spork/path
(defn file-dir-path
  [path]
  (when (empty? path)
    (break path))
  #
  (def os (or (dyn :os-override) (os/which)))
  (def bs-land (or (= :windows os) (= :mingw os)))
  (def sep (if bs-land `\` "/"))
  (if bs-land
    (when (peg/match ~(sequence :a `:\` -1) path)
      (break path))
    (when (= path "/")
      (break path)))
  #
  (def rev-path (string/reverse path))
  (def last-sep-idx (string/find sep rev-path))
  (def candidate
    (if-not last-sep-idx
      path
      (string/slice path 0
                    (dec (- (length path) last-sep-idx)))))
  #
  (if (not (string/find sep candidate))
    (string candidate sep)
    candidate))

(comment

  # an actual case we want to handle
  (file-dir-path "/etc/motd")
  # =>
  "/etc"

  # another actual case we want to handle
  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `C:\Windows\System32\taskmgr.exe`)))
  # =>
  `C:\Windows\System32`

  # everything below here are edge cases we don't care about...

  (file-dir-path "")
  # =>
  ""

  (file-dir-path "/")
  # =>
  "/"

  # differs from python and spork
  (file-dir-path "//")
  # =>
  "/"

  (file-dir-path "/etc")
  # =>
  "/"

  # sames as python's os.path.dirname, different from unix dirname
  (file-dir-path "/etc/")
  # =>
  "/etc"

  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `C:\Windows`)))
  # =>
  `C:\`

  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `A:\`)))
  # =>
  `A:\`

  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `Z:\`)))
  # =>
  `Z:\`

  )

(defn split-path
  [path]
  (def fd-path (file-dir-path path))
  (when (empty? path)
    (break @["" ""]))
  #
  (def idx (inc (string/find fd-path path)))
  #
  @[fd-path
    (string/slice path (+ idx (length fd-path)) -1)])

(comment

  # really the only case we care about
  (split-path "/etc/motd")
  # =>
  @["/etc" "motd"]

  )

(defn abspath?
  [path &opt bs-land]
  (def os (os/which))
  (default bs-land (or (= :windows os) (= :mingw os)))
  (if bs-land
    # https://stackoverflow.com/a/23968430
    # https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats
    (truthy? (peg/match ~(sequence :a `:\`) path))
    (string/has-prefix? "/" path)))

(defn touch
  [path]
  (with [f (file/open path :w+)]
    true))

