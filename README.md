The aim of this web server is to win the PONG benchmark at all costs.

The PONG benchmark is generally run as follows:

    $ httperf --hog -v --server 127.0.0.1 --port 8000 --uri / --num-conns=1000 --num-calls=1000 --burst-length=20 --rate=1000

Here is a sample session:

    httperf --verbose --hog --client=0/1 --server=127.0.0.1 --port=8000 --uri=/ --rate=1000 --send-buffer=4096 --recv-buffer=16384 --num-conns=1000 --num-calls=1000 --burst-length=20
    httperf: warning: open file limit > FD_SETSIZE; limiting max. # of open files to FD_SETSIZE
    httperf: maximum number of open descriptors = 1024
    Maximum connect burst length: 7
    
    Total: connections 1000 requests 1000000 replies 1000000 test-duration 4.970 s
    
    Connection rate: 201.2 conn/s (5.0 ms/conn, <=1000 concurrent connections)
    Connection time [ms]: min 1902.6 avg 3975.9 max 4345.9 median 4029.5 stddev 274.3
    Connection time [ms]: connect 1.3
    Connection length [replies/conn]: 1000.000
    
    Request rate: 201212.3 req/s (0.0 ms/req)
    Request size [B]: 62.0

    Reply rate [replies/s]: min 0.0 avg 0.0 max 0.0 stddev 0.0 (0 samples)
    Reply time [ms]: response 57.1 transfer 0.0
    Reply size [B]: header 38.0 content 4.0 footer 0.0 (total 42.0)
    Reply status: 1xx=0 2xx=1000000 3xx=0 4xx=0 5xx=0
    
    CPU time [s]: user 0.96 system 3.98 (user 19.3% system 80.1% total 99.4%)
    Net I/O: 20435.6 KB/s (167.4*10^6 bps)
    
    Errors: total 0 client-timo 0 socket-timo 0 connrefused 0 connreset 0
    Errors: fd-unavail 0 addrunavail 0 ftab-full 0 other 0

When reading the results it is import to check the 'Errors' lines and 'Replay status' to ensure that things are working correctly. You should see 0 erros and '2xx=1000000'.

