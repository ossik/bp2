## Copyright (c) 2010 Ossi Koivistoinen

## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:

## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.

## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.



# Node is created for each pathway and complex
setClass("bp2.nodes",
         representation(
                        # numerical rdf.id's of objects. Can be used to find
                        # corresponding object in bp2.all.objects table
                        ids = "vector",

                        # List of gene id strings in this particular node
                        proteins = "list",

                        # gene to node mapping
                        inverse.proteins = "list",
                        
                        # List of other nodes that are part of this node
                        sub.nodes = "list",

                        # list of nodes that share genes with or have genes controlled by this node
                        nodes.with.common.proteins = "list",
                        nodes.with.activated.proteins = "list",
                        nodes.with.inhibited.proteins = "list",
                        
                        # nodes and genes that are controlled by this node
                        activated.nodes = "list",
                        activated.proteins = "list",
                        inhibited.nodes = "list",
                        inhibited.proteins = "list",
                        
                        # union of sub.nodes, activated.nodes,
                        # inhibited.nodes and common.genes
                        all.followers = "list",

                        # union of inhibited.nodes and nodes.with.inhibited.gene
                        all.inhibited = "list"
                        ))

setMethod("show", "bp2.nodes", bp2.simple.show);
