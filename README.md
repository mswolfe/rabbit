rabbit
======

A small category oriented task tracking system.  Yeah, yeah, there's a ton
of task tracking programs out there, but all I wanted was a simple command line
tracking program.  Oh, and I wanted to write something in scheme.  Let me know
if you enjoy the program!  Feedback is always appreciated.

Overview
========

Rabbit requires at least a database path (given with --database).  After that
feel free to begin tasks (--begin [category]), switch tasks (--switch [category]),
end tasks (--end) or look at the task activities with --list or --overview.
--list will print all tasks that fall within the specified range
(day/week/month/year) and --overview will show the totals for all categories
that fall within the specified range (day/week/month/year).

You can always run the program with the -h or --help option to see the full list
of supported options.

Building
--------
* Linux - There is a build script in the bin directory called build.sh
* Mac - Nothing yet.
* Windows - Nothing yet.
 
Running
-------
* Linux - There is a run script in the bin directory called rabbit.sh
* Mac - Nothing yet.
* Windows - Nothing yet.
 
Code
====

Rabbit uses sqlite to store category and task information.  It's a fairly simple
database since I'm not an sql guru...

Two tables
* categories - category_id, name
* tasks - task_id, category_id, start, stop
 
Module: rabbit-db
-----------------
The rabbit-db (src/rabbit-db.rkt) module provides functions for accessing the
database.  The functions are mirrored after the sql statements they execute. So,
insert-task will execute an insert statement on the task table.  update, delete,
and select are architected in a similar fashion.  When the returned result is
fancier then a simple list the rabbit-db module tries to bundle it up into a
nice struct (see select-task(s) which returns a list of task structures).
Hopefully, this conversion doesn't slow down the system too much.

Module: rabbit
--------------
The rabbit (src/rabbit.rkt) module provides functions which take into account
task tracking logic.  We use zero in the stop field to indicate that the task
is currently being worked.  Functions like end-task use the zero stop field to
know which task to set the stop time of.  Switch task understands how to end
all current tasks and begin a new task.

Module: rabbit-common
---------------------
The rabbit-common (src/rabbit-common.rkt) module holds common structures and
conversion methods.  Currently, this module is fairly simple...

Module: rabbit-cli
------------------
The rabbit-cli (src/rabbit-cli.rkt) module is the command line interface to
the rabbit module.

Testing
-------
The test scripts in the src/test directory do not cover the entire code base.
I tried, but it's not perfect...