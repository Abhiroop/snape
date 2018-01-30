# snape

Parallelize your workload. Like magic.

![alt text](https://raw.githubusercontent.com/Abhiroop/snape/master/snape.png "Snape")

*As there is little foolish wand-waving
here, many of you will hardly believe this is magic. - Severus Snape*


### TODOS:

- [ ] Use Unagi Chan Queue and replace the naive queue
- [ ] Explore all LINQ/SQL operators and use them as a Task
- [ ] Move all global messages tasks etc to a more global namespace
- [ ] Make IO part of the monad transformer stack and see how to imporove performance of Writer monad etc.
- [ ] Must add **WORK STEALING** from peers
- [ ] Allow dynamic growth of task graph post compilation

### INSPIRATION

- Naiad - Timely Data Flow https://www.microsoft.com/en-us/research/publication/naiad-a-timely-dataflow-system-2/
- http://dask.pydata.org/en/latest/
