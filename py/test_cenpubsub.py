#!/usr/bin/python


import argparse
import os
import time

from mininet.net import Mininet
from mininet.topo import Topo
from mininet.link import TCLink
from mininet.log import setLogLevel
from mininet.util import waitListening


from cenpubsub import BROKER_PORT
from star import StarTopo


def test_cenpubsub(script_dir, log_dir, n=5, \
        broker_port="8888", sub_port="9999"):
    '''
    Test centralized pub/sub on a star topology (what else?)
    '''

    assert n > 2

    topo = StarTopo(n)
    net = Mininet(topo)
    net.start()

    net.pingAll()
    
    pubsub_prog = os.path.join(script_dir, 'stack exec linkmatch-exe -- ')

    
    # Setup schema file
    # TODO

    # Setup subscription files
    # TODO

    # Setup broker topo file
    # TODO


    def log_redirect_tail(name):
        return ' > ' + os.path.join(log_dir, name + '.out') \
            + ' 2> ' + os.path.join(log_dir, name + '.err')


    print 'mn: starting broker.. with ', broker_prog
    broker_node.cmd( \
        pubsub_prog,    
        '-b', # broker mode
        '--my_ip', broker_node.IP(),
        '--my_port', broker_port,
        '--schema', schema_path,
        '--topo', topo_path,
        log_redirect_tail(broker_node.name)
        )

    
    # Wait for broker to start
    time.sleep(1)


    # Setup subscribers
    print 'mn: starting hosts...'
    for host in net.hosts[2:]:
        host.cmd( \
            pubsub_prog,
            '-s',
            '--my_ip', host.IP()    
            '--my_port', sub_port,
            '--broker_ip', None # FIXME...
            '--schema', schema_path,
            
    
    time.sleep(1)

    
    pub_node = net.hosts[1]
    print 'mn: starting publisher'
    pub_node.cmd('%s %s %s > %s 2>%s &' % \
            (publisher_prog, broker_node.IP(), BROKER_PORT, \
            os.path.join(log_dir ,'pub.out'), \
            os.path.join(log_dir, 'pub.err')))

    # Wait for stuff to happen
    time.sleep(n)

    print 'killing broker...'
    broker_node.cmd('pkill broker.py') # FIXME robust?
    print 'broker cleaned up'

    for host in net.hosts[2:]:
        host.cmd('pkill subscriber.py') # FIXME robust?

    # FIXME: pub_node is hopefully dead already?
    print 'kiling publisher...'
    pub_node.cmd('pkill publisher.py')
    print 'publisher cleaned up'

    net.stop()
    

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='A small test')
    parser.add_argument('script_dir', type=str)
    parser.add_argument('log_dir', type=str)

    args = parser.parse_args()


    script_dir = os.path.abspath(args.script_dir)
    log_dir = os.path.abspath(args.log_dir)

    print script_dir
    print log_dir

    test_cenpubsub(script_dir, log_dir)
