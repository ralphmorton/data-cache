# data-cache
A very simple lib for caching IO stuff, with TTL and LRU. 

### Sample usage:

    -- Create a cache with a TTL of 20 seconds and max item count of 1000
    c <- createCache (Just 20) (Just 1000) (\key -> putStrLn key >> getLine)

    -- Fetch an item. Since the item doesn't exist in the cache the IO action will be invoked to retrieve it
    r1 <- fetch "test" c
    putStrLn $ "fetched: " ++ r1

    -- This key 'test' is now cached, so it will be returned without invoking the IO action
    r2 <- fetch "test" c
    putStrLn $ "fetched (cached): " ++ r2

    -- Wait for TTL to expire
    threadDelay 21000000

    -- Since the TTL has been exceeded, the key has been purged, and the IO action will be invoked again
    r3 <- fetch "test" c
    putStrLn $ "fetched: " ++ r3