### Polynomial Exact Schedulability and Infeasibility Test for Fixed-Priority Scheduling on Multiprocessor Platforms: Experiments

This repository represent experiments for evaluating the schedulability performance of our algorithms 1 and 2 of the paper "Polynomial Exact Schedulability and Infeasibility Test for Fixed-Priority Scheduling on Multiprocessor Platforms".

The repository has the following structure. The `SchedulingTestingExperiments.lsp` file contains program code used in the experiments, and the `experiments` paper contains the results of the experiments.

The `SchedulingTestingExperiments.lsp` file has the following structure.

Section `; Data structures` describes data structures such as `task` and `taskset`.

Section `; Algorithms` contains program code of algorithms used in the experiments.

Section `; Library for generating and checking datasets` contains functions  of generating datasets (lists of objects of the `taskset` datatype), serializing datasets, running algorithms on datasets, evaluating the performance of algorithms, serializing the results of algorithms, etc. These functions provides software infrastructure for such and similar experiments.

Subsection `;; Auxiliary functions` contains auxiliary functions (in particular, the implementation of the UUnifast algorithm for distributing the total utilization of a set of tasks to separate tasks from this set.).

Subsection `;; Serializing datasets` contains functions of serializing datasets to Lisp files. Running the Lisp code of such a file restores the dataset in RAM.

Subsection `;; Applying algorithms` contains functions applying algorithms implementing schedulability/infeasibility tests to datasets.

Subsection `;; Serializing algorithm outputs` contains functions of serializing algorithms outputs to csv files with statistics collection.

Subsections `;; Dataset1`, `;; Dataset2`, `;; Dataset3` describes implementation of different strategies of generating datasets. All strategies generate datasets satisfying the constraint `Di = Ti` for all tasks of task sets from these datasets. Strategies in `;; Dataset1` and `;; Dataset2`are based on the UUnifast algorithm. Strategy in `;; Dataset1` generates datasets satisfying the following extra constraints: (i) `N = M + 1`, (ii) task sets in these datasets are deadline monotonic.

Section `; Initializing` initializes parameters of generated datasets such as sets `u-list-val` of utilizations, sets ` m-list-val` of processor numbers, and sets `uc-list-val` of utilization classes. An utilization class is a range of utilization and is described by its representative (an utilization). For example, `uc-list-val = (0.2 0.4 0.6 0.8 1.0)` ncludes five representatives `0.2`, `0.4`, `06.`, `0.8` and `1.0` describing utilization classes `[0.0, 0.2)`, `[0.2, 0.4)`, `[0.4, 0.6)`, `[0.6, 0.8)`, and `[0.8, 1.0)`, respectively.

Section `; Experiments` contains scripts of experiments. Running the script allows you to reproduce the corresponding experiment.

Subsection `;; Experiment1` contains the experiment comparing algorithms `Alg1`, `LeeShin2014`, and `BaekLee2020` in terms of successful schedulability tests and operation time spent on their work.

Subsection `;; Experiment2` contains the experiment with the `Alg2` algorithm collecting statistics of its work in terms of successful infeasibility tests and its operation time.

The `Experiments` paper includes the following files.

The files `dataset1.lsp` and `dataset2.lsp` contain datasets generating in experiments 1 and 2, respectively.

The files `ALg1-LeeShin2014-BaekLee2020*.*`, `Alg1*.*`, `LeeShin2014*.*` and `BaekLee2020*.*` contain statistics of results of the experiment 1 in various formats.

The files `ALg2*.*` contain statistics of results of the experiment 2 in various formats.
