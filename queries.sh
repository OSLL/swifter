#!/bin/bash

#Tests riak four-node cluster. 
#Type make devrel in source directory of riak then put
#script and image.jpg, Programming.pdf at ../riak-1.2.*/dev directory

#starting riak nodes
dev1/bin/riak start
dev2/bin/riak start
dev3/bin/riak start
dev4/bin/riak start

#joining nodes
dev2/bin/riak-admin cluster join dev1@127.0.0.1
dev3/bin/riak-admin cluster join dev1@127.0.0.1
dev4/bin/riak-admin cluster join dev1@127.0.0.1

#creating cluster of four nodes
dev1/bin/riak-admin cluster plan
dev1/bin/riak-admin cluster commit

#PUT files then GET them from storage  
curl -X PUT http://127.0.0.1:8091/riak/images/image.jpg -H "Content-Type: image/jpeg" --data-binary @image.jpg
curl -X PUT http://127.0.0.1:8091/riak/docs/erlang.pdf -H "Content-Type: application/pdf" --data-binary @Programming.pdf
curl -o "retrieved_image.jpg" http://127.0.0.1:8091/riak/images/image.jpg#
curl -o "book.pdf" http://127.0.0.1:8091/riak/docs/erlang.pdf

#DELETE files from storage
curl -X DELETE http://127.0.0.1:8091/riak/images/image.jpg
curl -X DELETE http://127.0.0.1:8091/riak/docs/erlang.pdf

