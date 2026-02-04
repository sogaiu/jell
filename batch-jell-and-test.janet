(def repos-root "./repos")

# tweak these to use a project's jell or some other versions
(def jell-path
  # using the version local the project
  "./bin/jell"
  # using the version on PATH
  #"jell"
  )

# tweak these to use a project's niche or some other versions
(def niche-path
  # using the version local the project
  "./bin/niche.janet"
  # using the version on PATH
  #"niche"
  )

########################################################################

(defn print-boundary
  [&opt len]
  (default len 60)
  (print (string/repeat "=" len)))

(defn tally-ecodes
  [results]
  (def by-ecode @{})
  #
  (each [proj-name ecode] results
    (def ps (get by-ecode ecode @[]))
    (put by-ecode ecode ps)
    (array/push ps proj-name))
  #
  by-ecode)

(defn print-ecodes
  [by-ecode]
  (each ecode (sort (keys by-ecode))
    (print "exit-code: " ecode)
    (each p (sort (get by-ecode ecode))
      (print " " p))))

########################################################################

(def proj-dirs (os/dir repos-root))

(def dir (os/cwd))

(def results @[])
(def no-jell @[])
(def no-niche @[])

(def start-time (os/clock))

# run jell and jeep test for each project and collect exit codes, etc.
(each proj-name (sort proj-dirs)
  (defer (os/cd dir)
    (def proj-dir (string repos-root "/" proj-name))
    (when (= :directory (os/stat proj-dir :mode))
      (os/cd proj-dir)
      (pp (string repos-root "/" proj-name))
      (def has-jell (os/stat "bin/jell" :mode))
      (if (not has-jell)
        (array/push no-jell proj-name)
        (let [j-ecode (os/execute [jell-path] :px)]
          (assertf (= 0 j-ecode)
                   "exit code non-zero for jell: %d" j-ecode)
          (def has-niche (os/stat "bin/niche.janet"))
          (if (not has-niche)
            (array/push no-niche proj-name)
            (do
              (def ecode (os/execute [niche-path] :px))
              (array/push results [proj-name ecode]))))))))

(print-boundary)

(def by-ecode (tally-ecodes results))

# display exit codes
(print-ecodes by-ecode)

(print-boundary)

# display projects for which jell was not detected
(when (not (empty? no-jell))
  (print "no jell detected")
  (each nj no-jell
    (print " " nj))
  (print-boundary))

# display projects for which niche was not detected
(when (not (empty? no-niche))
  (print "no niche detected")
  (each nn no-niche
    (print " " nn))
  (print-boundary))

(printf "Total project(s): %d" (length proj-dirs))
(printf "Total processing time: %.02f secs" (- (os/clock) start-time))

