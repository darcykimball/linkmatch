#!/usr/bin/python


import json

from spanning_tree import make_spanning_tree, assign_roles, switch_hosts



def broker_topo(net, roles=None):
    '''
    Construct a ready-to-pack-to-json to broker daemon dict representation of
    a topology.
    '''

    if roles is None:
        roles = assign_roles(topo)

    
    topod = {
        'children':

    
    }

    pass


if __name__ == '__main__':
    topo = make_spanning_tree()

    hosts_map = switch_hosts(topo)
    print (hosts_map)

    roles = assign_roles(topo)
    print roles
