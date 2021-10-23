# Debugger Plugin

A plugin that a creates a log file which can be compiled to a graph for display
with `graphviz` or some other means. Contrary to traditional debug logging
where all output is interleaved into a flat sequence of lines, the graph
structure produced by this plugin takes into account the actual flow of the
program with individual function calls represented as edges in the graph.
