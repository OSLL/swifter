#! /bin/bash
RIAK=riak-1.2.1.tar.gz
wget http://downloads.basho.com.s3-website-us-east-1.amazonaws.com/riak/1.2/1.2.1/$RIAK

if [ $? -eq 0 ]
then
   tar zxvf $RIAK
fi
exit 0
