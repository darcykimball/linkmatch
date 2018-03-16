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
    Construct a pseudo-spanning tree.
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


def tree_as_dict(topo, broker_links=None, root='s1'):
    '''
    Convert switch topology to a dict representation
    '''
    
    treed = dict()

    # Get just the broker links for top-level call
    if broker_links is None:
        broker_links = set((n1, n2) for n1, n2 in topo.links() \
                if topo.isSwitch(n1) and topo.isSwitch(n2))


    children = list()
    if len(broker_links) > 0:
        # Get outgoing edges from root
        edges = [(n1, n2) for n1, n2 in broker_links \
                if root == n1 or root == n2]
        print edges
        
        for n1, n2 in edges:
            broker_links.discard((n1, n2))

            if n1 == root:
                children.append(tree_as_dict(topo, broker_links, n2))
            else:
                children.append(tree_as_dict(topo, broker_links, n1))
            

    treed[root] = children

    return treed



def switch_hosts(topo):
    '''
    Get the directly-connected hosts to each switch in a topology.
    '''

    hosts_map = dict([(s, list()) for s in topo.switches()])
    
    for node1, node2 in topo.links():
        if topo.isSwitch(node1):
            if not topo.isSwitch(node2):
                hosts_map[node1].append(node2)
        if topo.isSwitch(node2):
            if not topo.isSwitch(node1):
                hosts_map[node2].append(node1)

    for hosts in hosts_map.values():
        hosts.sort()

    return hosts_map



def assign_roles(topo, root='s1'):
    '''
    Assign each host in a tree topology to a broker, subscriber, or publisher
    role.
    Returns the info in a dict mapping switches (1 per broker node) to a dict
    of host rules
    '''

    hosts_map = switch_hosts(topo)
    
    # We need to be able to select at least one of each role for a given group
    for group in hosts_map.values():
        assert len(group) >= 3

    
    all_roles = dict()
    for switch, group in hosts_map.iteritems():
        roles = dict()
        if switch == root:
            roles[group[0]] = 'publisher'
            roles[group[1]] = 'broker'
            for host in group[2:]:
                roles[host] = 'subscriber'
        else:
            roles[group[0]] = 'broker'
            for host in group[1:]:
                roles[host] = 'subscriber'

        all_roles[switch] = roles
    
    return all_roles


def test_spanning_tree():
    topo = make_spanning_tree()

    net = Mininet(topo)

    net.start()

    net.pingAll()

    net.stop()


if __name__ == '__main__':
    setLogLevel('info')
    
    test_spanning_tree()
