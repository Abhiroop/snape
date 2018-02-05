# snape

Parallelize your workload. Like magic.

![alt text](https://raw.githubusercontent.com/Abhiroop/snape/master/snape.png "Snape")

*As there is little foolish wand-waving
here, many of you will hardly believe this is magic. - Severus Snape*


### TODOS:

- [x] Use Unagi Chan Queue and replace the naive queue
- [ ] Explore all LINQ/SQL operators and use them as a Task
- [ ] Move all global messages tasks etc to a more global namespace
- [x] Make IO part of the monad transformer stack
- [ ] Performance analysis of Writer monad.
- [ ] Must add **WORK STEALING** from peers
- [ ] Allow dynamic growth of task graph post compilation
- [ ] Design a graph IR to which the frontend compiles down to

### INSPIRATION

- Naiad - Timely Data Flow https://www.microsoft.com/en-us/research/publication/naiad-a-timely-dataflow-system-2/
- http://dask.pydata.org/en/latest/
