---
title: "Bounded_int_table"
uuid: a7090c8a-6a1f-3a31-176b-e35da7487f2b
---

A `Bounded_int_table` is a table whose keys can be mapped to integers
in a fixed range, `0` ... `num_keys - 1`, where `num_keys` is
specified at table-creation time.  The purpose of `Bounded_int_table`
is to be faster than `Hashtbl` in situations where one is willing to
pay a space cost for the speed.
