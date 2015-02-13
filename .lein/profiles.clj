;; User profiles.
{:user {:aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :dependencies [[slamhound "1.5.5"]]
        :plugins [[lein-ancient "0.6.2"]
                  [cider/cider-nrepl "0.9.0-SNAPSHOT"]]
        :jvm-opts [(when (= "Mac OS X" (System/getProperty "os.name")) "-Dapple.awt.UIElement=true")]}}
