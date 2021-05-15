(ns ^:figwheel-no-load aeons-end.dev
  (:require
    [aeons-end.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
