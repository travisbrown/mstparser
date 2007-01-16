#!/bin/sh
. mst-env
$JAVA_CMD -Xrunhprof mstparser.DependencyParser $@
