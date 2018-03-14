#!/usr/bin/python


import argparse
import os
import time

from mininet.net import Mininet
from mininet.topolib import TreeTopo, TreeNet
from mininet.link import TCLink
from mininet.log import setLogLevel


def make_spanning_tree(broker_depth=2):
    '''
    Construct a pseudo-spanning tree
    '''

    tree = TreeTopo(depth=broker_depth, fanout=3)

    # Add extra nodes for publishers and subscribers to each switch without
    # hosts
    bare_switches = set(tree.switches())
    for src, dest in tree.links():
        if tree.isSwitch(src) and not tree.isSwitch(dest):
            bare_switches.discard(src)
        if tree.isSwitch(dest) and not tree.isSwitch(src):
            bare_switches.discard(dest)


    print 'bare switches: ', bare_switches

    nHost = len(tree.hosts()) + 1
    for switch in bare_switches:
        for i in xrange(3):
            host = 'h%s' % nHost
            tree.addHost(host)
            nHost = nHost + 1
            tree.addLink(host, switch)


    return tree


def test_spanning_tree():
    topo = make_spanning_tree()

    net = Mininet(topo)

    net.start()

    net.pingAll()

    net.stop()


if __name__ == '__main__':
    setLogLevel('info')

    test_spanning_tree()
