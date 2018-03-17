#!/usr/bin/python -u 


import argparse
import json
import os
import time

from mininet.net import Mininet
from mininet.log import setLogLevel

from spanning_tree import *
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


    rootIP = net.get(root).IP()

    
    # Depth first for no particular reason
    children = list()
    host_subs = dict()
    for child in treed[root]:
        c_topod, c_host_subs = broker_topo( \
                net, schema_vals, child, roles, child.keys()[0])
        children.append(c_topod)

        for h, s in c_host_subs.iteritems():
            host_subs[h] = s

    
    subIPs = list()
    for host, role in roles[root].iteritems():
        if role == 'subscriber':
            preds = [predicate(pred) for pred in random_predicate(nsub, schema_vals)]
            subIPs.append([net.get(host).IP(), preds])
            host_subs[host] = preds 


    topod = {   
        'rootIP': rootIP,
        'children': children,
        'subIPs': subIPs,
        }

    return topod, host_subs


def test_linkmatch(script_dir, log_dir, depth=2):
    topo = make_spanning_tree(1)


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

    schema = event_schema(dict(schema_vals.keys()))


    net = Mininet(topo)

    # Set up broker topology
    btopo, host_subs = broker_topo(net, schema_vals)
    print btopo


    with open(os.path.join(log_dir, 'topo.json'), 'w+') as f:
        json.dump(btopo, f, indent=2)


    # Set up daemon config files
    schema_path = os.path.join(log_dir, 'schema.json')
    with open(schema_path, 'w+') as f:
        json.dump(schema, f, indent=2)
    
    print host_subs

    for subscriber, preds in host_subs.iteritems():
        with open(os.path.join(log_dir, subscriber + '_preds.json'), 'w+') as f:
            json.dump(preds, f, indent=2)


    # Let's go
    net.start()

    net.pingAll()   

    net.stop()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='A small test')
    parser.add_argument('script_dir', type=str)
    parser.add_argument('log_dir', type=str)      

    args = parser.parse_args()                          


    script_dir = os.path.abspath(args.script_dir)
    log_dir = os.path.abspath(args.log_dir) 
    test_linkmatch(script_dir, log_dir)
