#!/usr/bin/python


import json

from mininet.net import Mininet
from spanning_tree import make_spanning_tree, assign_roles, switch_hosts, \
        tree_as_dict

from cfgjson import *


def broker_topo(net, schema_vals, treed=None, roles=None, root='s1', nsub=1):
    '''
    Construct a ready-to-pack-to-json to broker daemon dict representation of
    a topology, with randomly generated subscriptions for subscribers.
    '''

    topo = net.topo

    if roles is None:
        roles = assign_roles(topo)

    if treed is None:
        treed = tree_as_dict(topo)

    
    print 'constructing broker topology with roles:\n', roles
    print 'constructing broker topology with switch tree:\n', treed


    rootIP = net.get(root).IP()

    children = [broker_topo(net, schema_vals, child, roles, child.keys()[0]) \
            for child in treed[root]]
    
    subIPs = list()
    for host, role in roles[root].iteritems():
        if role == 'subscriber':
            preds = [pred for pred in random_predicate(nsub, schema_vals)]
            subIPs.append([net.get(host).IP(), preds])



    topod = {   
        'rootIP': rootIP,
        'children': children,
        'subIPs': subIPs,
        }

    return topod


if __name__ == '__main__':
    topo = make_spanning_tree()

    hosts_map = switch_hosts(topo)
    print (hosts_map)

    roles = assign_roles(topo)
    print roles

    treed = tree_as_dict(topo)
    print treed


    schema_vals = {                                                          
        ('weight', 'Number') : [10, 20, 30, 40],                             
        ('temp', 'Number') : [100, 80, 90, 70],                              
        ('breed', 'Str') : ['corgi', 'doxie', 'pitbull'],                    
        ('color', 'Str') : ['red', 'brown', 'black', 'spotted'],             
        }                                                                    


    net = Mininet(topo)


    btopo = broker_topo(net, schema_vals)
    print json.dumps(json.dumps(btopo))
