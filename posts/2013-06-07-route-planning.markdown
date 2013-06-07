---
comments: true
date: 2013-06-07 20:00:00
layout: post
slug: test
title: Route Planning
tags: Route Planning, Route Proposal
---

In [the repository](https://bitbucket.org/dibblego/ride), you will find [a Haskell script](https://bitbucket.org/dibblego/ride/src/master/static/route/Route.hs) for generating a URL containing waypoints. The URL points to
[the Open Source Routing Machine (OSRM)](http://map.project-osrm.org/), which then produces a route that visits each
waypoint.

There is a problem in that some of the [OpenStreetMap (OSM)](http://osm.org) tracks are not marked as having vehicular
access. This means that OSRM will not use them for routing. This is especially prevalent in western Queensland and the
Simpson desert. Therefore, although I have created many route proposals, I am waiting for the OSM map to update on OSRM
so that it routes along vehicle tracks.

If you would like to propose a route, please use [the OSRM website](http://map.project-osrm.org/), generate a URL and
submit it to [the mailing list](http://groups.google.com/group/au-ride-2014). You can also do this to modify an existing
route proposal.
