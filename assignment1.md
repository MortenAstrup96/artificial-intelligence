1.0 Find nearest package in time
1.1 Move towards package undtil at package (Taking shortest path)
1.2 Pick up package

2.0 Find delivery destination
2.1 Move towards destination (shortest path)
2.2 Deliver package

TrafficMatrix:
- hroads (9x10 matrix)
- vroads (10x9 matrix)

CarInfo:
- x
- y
- load (0 / package)
- mem (don't use me)

PackageInformation
- 5 columns + 1 row pr. package
- Each row is
-- a (pickup)
-- b (pickup)
-- x (dest)
-- y (dest)
-- status (0 / 1)