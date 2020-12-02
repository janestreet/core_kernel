---
title: "Weak_hashtbl"
uuid: a812b4e1-f682-3a43-d4b3-20e8828154cd
---

A single-module library with a hashtable that keeps a weak pointer to
each key's data and uses a finalizer to detect when the data is no
longer referenced (by any non-weak pointers).
