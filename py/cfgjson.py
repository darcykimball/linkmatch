#!/usr/bin/python


import json
import random


def event_schema(attrs):
    '''
    Utility fn for making JSON-serializable event schema    
    Argument is a dict of names to types (either 'Str' or 'Number'), to
    match aeson's encoding of Haskell types.
    '''

    return {name: {'tag': typ, 'contents': default_val(typ)} \
        for name, typ in attrs.iteritems()}


def default_val(typ):
    if typ == 'Str':
        return ""
    if typ == 'Number':
        return 0

    raise ValueError
        

def predicate(attrs):
    '''
    Utility fn for JSON-serializable subscription predicates (lists of attrs)
    Argument is an iterable of (name, type, value)
    '''
    
    return [{'_attrVal': {'tag': typ, 'contents': val}, \
        '_attrName': name} for (name, typ, val) in attrs]


def random_predicate(n, schema_values, seed=0xDEADBEEF):
    '''
    Generator (inf) for random subscription predicates under a given schema
    Argument is a dict from attribute (name, type) to possible values
    '''
 
    random.seed(seed)

    for i in xrange(n):
        pred = list()
        for (name, typ), vals in schema_values.iteritems():
            chosen = random.choice(vals + [None])
            if chosen is not None:
                pred.append((name, typ, chosen))
    
        yield pred


if __name__ == '__main__':
    some_schema = {
        'weight': 'Number',
        'temp': 'Number',
        'breed': 'Str',
        'color': 'Str',
        }

    es = event_schema(some_schema)
    print json.dumps(es)
    print json.dumps(json.dumps(es))



    schema_vals = {
        ('weight', 'Number') : [10, 20, 30, 40],
        ('temp', 'Number') : [100, 80, 90, 70],
        ('breed', 'Str') : ['corgi', 'doxie', 'pitbull'],
        ('color', 'Str') : ['red', 'brown', 'black', 'spotted'], 
        }

    print schema_vals
    
    for pred in random_predicate(5, schema_vals):
        print pred
