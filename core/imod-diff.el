;;; -*- lexical-binding: t -*-

;;; Code:

(xzz ediff
  :config
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-highlight-all-diffs 'nil
          ediff-diff-options "-w"))

(xzz ztree
  "Use `ztree-diff' to diff directories."
  :ref "fourier/ztree"
  :config
  (setopt ztree-draw-unicode-lines t
          ztree-diff-additional-options nil) ; '("-w" "-i")
  (add-to-list 'ztree-diff-filter-list "^~"))

(transient-define-prefix im/transient-ediff () ; C-c d e
  "Launch Ediff in all it's variants"
  [["2 Way"
    ("b"  "Buffers"        ediff-buffers)
    ("f"  "Files"          ediff-files)
    ("d"  "Directories"    ediff-directories)
    ("c"  "Buffer vs File" ediff-current-file)
    ("~"  "File vs Backup" ediff-backup)
    ]
   ["3 Way"
    ("3b" "Buffers"        ediff-buffers3)
    ("3f" "Files"          ediff-files3)
    ("3d" "Directories"    ediff-directories3)
    ]
   ["Patches"
    ("pb" "Buffer"         ediff-patch-buffer)
    ("pf" "File"           ediff-patch-file)
    ]
   ["Regions"
    ("rl" "Linewise"       ediff-regions-linewise)
    ("rw" "Wordwise"       ediff-regions-wordwise)
    ]
   ["Windows"
    ("wl" "Linewise"       ediff-windows-linewise)
    ("ww" "Wordwise"       ediff-windows-wordwise)
    ]
   ])
