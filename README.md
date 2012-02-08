bubba (Benchmarking Using Browser-Based Animation)
==================================================

Bubba leverages erlang, yaws and d3.js to animate and compare the execution of different algorithms. Initially, it measures embedded erlang code but the design allows calling out to other languages using erlang NIFs.

Bubba is released under the New BSD Simplified license.


Included software
=================

This project is built with dependencies on other open source software projects.

The following software is included in the deployed application bundle:

  * dk_yaws (0.1.1)
  * d3.js

When building, ensure that dk_yaws is not present in the same directory as bubba. I use the following working directory layout when developing on both of these projects:

  * ~/Gitdeps/dk_yaws
  * ~/Gitdk/bubba

This restriction exists because the reltool.config file uses "../.." as a library pathname for finding the bubba project, but it gets confused when it can see "../deps/dk_yaws" and "../../dk_yaws" and causes a build error.
