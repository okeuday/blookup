Erlang Binary Lookup Data Structures
====================================

Warning: Justifying the usage of a global binary key/value dictionary
when compared to more efficient access of data through ETS should be
difficult (i.e., a unique circumstance).

These data structures allow data to be shared between processes without
creating separate copies of the data for each process to minimize memory usage.
The dict API is used for the data structures (add functions as you need them).

Both lookup and storage in the data structures is slow when compared to
other Erlang data structures.  blookupf and blookupv are roughly
27x slower than maps in 18.1 when doing lookups with a total of 10000 keys.
The blookupf data structure is quicker to store with when compared to
blookupv, with the speed difference increasing by a factor of 3 when
increasing the number of keys by a factor 10 (when the number of keys is
greater than 100 (see Note 1)).  However, blookupf has an absolute limit in
bytes that is set upon creation (with blookupf:new/1) which limits the
amount of data consumed by a key/value pair.

Due to the slow access times, the only usage of the data structures would
be for large amounts of data (e.g., caching of large files) to avoid a
greater amount of latency when accessing the data without the data structures.

The data structures keep all the data in large binaries due to binaries being
the only data type that is reference counted in Erlang.  The large
binary usage avoids any extra copying that would create extra memory
consumption, so memory is conserved for a large amount of data at the expense
of slower access times.

For specifying both the size of the key and the size of the value, see the
[bisect](https://github.com/knutin/bisect) data structure.

###Note 1:

This assumes the blookupv stores are not replacing key/value pairs of the
same total size.  If the sizes of the new and old key/value pairs are
similar, set `BLOCK_SIZE_BYTES` higher to increase the chance of the
replacement being the same total size (the difference needs to be less than the
`BLOCK_SIZE_BYTES` value).

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

MIT License
