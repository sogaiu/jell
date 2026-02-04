#! /usr/bin/env janet

(use ./sh-dsl)

########################################################################

(prin "* running jell from source...") (flush)
(def main-exit ($ janet ./src/main.janet))
(assertf (zero? main-exit)
         "main.janet exited: %d" main-exit)
(print "done")

########################################################################

(prin "* running jell...") (flush)
(def jell-exit ($ janet ./bin/jell))
(assertf (zero? jell-exit)
         "jell exited: %d" jell-exit)
(print "done")

########################################################################

(print "* running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

########################################################################

(print "* running jell batch tests...")
(def batch-exit ($ janet ./batch-jell-and-test.janet))
(assertf (zero? batch-exit)
         "batch testing exited: %d" batch-exit)
(print "done")

########################################################################

(print "* updating README...")
(def readme-update-ext ($ janet jell -h > README))
(assertf (zero? readme-update-ext)
         "updating README exited: %d" readme-update-ext)
(print "done")

