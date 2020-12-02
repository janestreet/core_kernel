---
title: "Thread_pool_cpu_affinity"
uuid: 5666165b-083a-33dd-2f0b-b2e15b7e2935
---

A very small library that defines a type for configuring how
`Thread_pool` affinitizes worker threads to CPUs.  This is a separate
library from `Thread_pool` because this only depends on `Core_kernel`,
and hence can be used in `Async_kernel`.
