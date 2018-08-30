# TST Candidate Take Home Test

This was the take home programming assignment for Tim Liu

# Compilation and Execution
There were are a lot of different ways of packaging and running code, but I chose a method which was quick and fairly straight forward.

First ensure that scalac is installed on the command line.  I installed it on my mac using:

`brew install scala`

The version that this was tested on was 2.12.6

Now, make sure you are in the root directory of this repository and run:

`scalac src/main/scala/timliu/*`

Once the compilation is complete, now you can execute the problems by running:

`scala -classpath . timliu.Problem1`
and
`scala -classpath . timliu.Problem2`

The expected output should appear as per the assignment
