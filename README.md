streaming
=========

Contents
--------

§ 1.  The freely generated stream on a streamable functor

§ 2.  A freely generated stream of individual Haskell values is a Producer, Generator or Source

§ 3.  `Streaming.Prelude`

§ 4.  Mother's `Prelude` v. `Streaming.Prelude`

§ 5.  How come there's not one of those fancy "ListT done right" implementations in here?

§ 6.  Didn't I hear that free monads are a dog from the point of view of efficiency?

§ 7.  Interoperation with the streaming-io libraries

§ 8.  Where can I find examples of use?

§ 9.  Problems

§ 10. Implementation and benchmarking notes


§ 1.  The freely generated stream on a streamable functor

`Stream` can be used wherever [FreeT](https://hackage.haskell.org/package/free-4.12.1/docs/Control-Monad-Trans-Free.html) is used. The compiler's standard range of optimizations work better for operations written in terms of `Stream`. `FreeT f m r` and `Stream f m r` are of course extremely general, and many functor-general combinators are exported by the general module `Streaming`. 


§ 2. A freely generated stream of individual Haskell values is a Producer, Generator or Source
---------------------------------------------------------------------------------------------------------

In the applications we are thinking of, the general type `Stream f m r` expresses a succession of steps arising in a monad `m`, with a shape determined by the 'functor' parameter `f`, and resulting in a final value `r`. In the first instance you might read `Stream` as `Repeatedly`, with the understanding that one way of doing something some number of times, is to do it no times at all. 

Readings of `f` can be wildly various. Thus, for example, 

     Stream Identity IO r
     
is the type of an indefinitely delayed `IO r`,

But given readings of `f` and `m` we can, for example, always consider the type `Stream (Stream f m) m r`, in which steps of the form `Stream f m` are joined end to end. Such a stream-of-streams might arise in any number of ways; a crude (because hyper-general) way would be with

    chunksOf :: Monad m, Functor f => Int -> Stream f m r -> Stream (Stream f m) m r
    
and we can always rejoin such a stream with

    concats ::  Monad m, Functor f =>  Stream (Stream f m) m r -> Stream f m r

But things can be chunked and concatenated in that sense; indeed these functions are modeled on those in `pipes-group`. In our [representation](https://hackage.haskell.org/package/streaming-utils-0.1.4.0/docs/Streaming-Pipes.html#v:concats), these have the types

    chunksOf :: Monad m => Int -> Producer a m r -> Stream (Producer a m) m r
    concats ::  Monad m =>  Stream (Producer a m) m r -> Producer a m r
    
Of course, as soon as you grasp the general form of *succession*, you are already in possession of the most basic concrete form: a simple *succession of individual Haskell values* one after another. This is just `Stream ((,) a) m r`. Here we prefer `Stream (Of a) m r`, strictifying the left element of the pair with 

    data Of a r = !a :> r deriving Functor

Either way, the pairing just links the present element with the rest of the stream. The primitive `yield` statement just expresses the pairing of the yielded item with the rest of the stream; or rather it is itself the trivial singleton stream. 

    yield 17  :: Stream (Of Int) IO ()

`Streaming.Prelude` is focused on the manipulation of this all-important stream-form, which appears in the streaming IO libraries under titles like:

    io-streams: Generator a r
    pipes:      Producer a m r
    conduit:    ConduitM () o m r
    streaming:  Stream (Of a) m r

The only difference is that in `streaming` the simple generator or producer concept is formulated explicitly in terms of the *general* concept of successive connection. But *this is a concept you need and already possess anyway*, as your comprehension of the streaming ABCs showed.

The special case of a *stream of individual Haskell values* that simply *comes to an end without a special result* is variously expressed thus:

    io-streams: InputStream a 
    pipes:      Producer a m ()
    conduit:    Source m a
    machines:   SourceT m a (= forall k. MachineT m k a)
    streaming:  Stream (Of a) m ()

§ 3. `Streaming.Prelude`
-------------------------

`Streaming.Prelude` closely follows `Pipes.Prelude`. But since it restricts itself to use only of the general idea of streaming, it cleverly *omits the pipes*:

    ghci> S.stdoutLn $ S.take 2 S.stdinLn
    let's<Enter>
    let's
    stream<Enter>
    stream

Here's a little *connect and resume*, as the streaming-io experts call it:

    ghci> rest <- S.print $ S.splitAt 3 $ S.each [1..10]
    1
    2
    3
    ghci> S.sum rest
    49

Somehow, we didn't even need a four-character operator for that, nor advice about best practices! - just ordinary Haskell common sense.

§ 4. Mother's `Prelude` v. `Streaming.Prelude`
------------------------------------------------

The effort of `Streaming.Prelude` is to leverage the intuition the user has acquired in mastering `Prelude` and `Data.List` and to elevate her understanding into a general comprehension of effectful streaming transformations. Unsurprisingly, it takes longer to type out the signatures. It cannot be emphasized enough, thought, that *the transpositions are totally mechanical*:

    Data.List.Split.chunksOf :: Int -> [a]          -> [[a]]
    Streaming.chunksOf       :: Int -> Stream f m r -> Stream (Stream f m) m r

    Prelude.splitAt   :: Int -> [a]          -> ([a],[a])
    Streaming.splitAt :: Int -> Stream f m r -> Stream f m (Stream f m r)

These concepts are "functor general", in the jargon used in the documentation, and are thus exported by the main `Streaming` module. Something like `break` requires us to inspect individual values for their properties, so it is found in the `Streaming.Prelude`

    Prelude.break           :: (a -> Bool) -> [a]               -> ([a],[a])
    Streaming.Prelude.break :: (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)

It is easy to prove that *resistance to these types is resistance to effectful streaming itself*. I will labor this point a bit more below, but you can also find it developed, with greater skill, in the documentation for the pipes libraries.

§ 5. How come there's not one of those fancy "ListT done right" implementations in here?
------------------------------------------------------------------------------------------

The use of the final return value appears to be a complication, but in fact it is essentially contained in the idea of effectful streaming. This is why this library does not export a \_ListT done right/, which would be simple enough - following `pipes`, as usual:

    newtype ListT m a = ListT (Stream (Of a) m ())

The associated monad instance would wrap

    yield :: (Monad m)            => a -> Stream (Of a) m ()
    for   :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m ()) -> Stream f m r

To see the trouble, consider [this signature](http://hackage.haskell.org/package/list-t-0.4.5.1/docs/ListT.html#v:splitAt) for splitting a ListT very much done right. Here's what becomes of [chunksOf](http://hackage.haskell.org/package/list-t-0.4.5.1/docs/src/ListT.html#slice). As long as we are trapped in some sort of ListT, however much rightly implemented, these operations can't be made to stream; something like a list must be accumulated. Similarly, try to imagine adding a `splitAt` or `lines` function to [this API](https://hackage.haskell.org/package/list-t-text-0.2.0.2/docs/ListT-Text.html). It would accumulate strict text forever, just as [this does](https://hackage.haskell.org/package/io-streams-1.3.2.0/docs/System-IO-Streams-ByteString.html#v:lines) and [this doesn't](https://hackage.haskell.org/package/pipes-bytestring-2.1.1/docs/src/Pipes-ByteString.html#lines) and [this doesn't](https://hackage.haskell.org/package/streaming-bytestring-0.1.0.6/docs/Data-ByteString-Streaming-Char8.html#v:lines) The difference is simply that the latter libraries operate with the general concept of streaming, and the whole implementation is governed by it. The attractions of the various "`ListT` done right" implementations are superficial; the concept belongs to logic programming, not stream programming.

Note similarly that you can write a certain kind of [take](http://hackage.haskell.org/package/machines-0.5.1/docs/Data-Machine-Process.html#v:taking) and [drop](http://hackage.haskell.org/package/machines-0.5.1/docs/Data-Machine-Process.html#v:dropping) with the `machines` library - as you can even with a "`ListT` done right". But I wish you luck writing `splitAt`! Similarly you can write a [getContents](http://hackage.haskell.org/package/machines-io-0.2.0.6/docs/System-IO-Machine.html); but I wish you luck dividing the resulting bytestream on its lines. This is - as usual! - because the library was not written with the general concept of effectful succession or streaming in view. Materials for sinking some elements of a stream in one way, and others in other ways - copying each line to a different file, as it might be, but without accumulation - are documented within. So are are myriad other elementary operations of streaming io.

§ 6. Didn't I hear that free monads are a dog from the point of view of efficiency?
-------------------------------------------------------------------------------------

We noted above that if we instantiate `Stream f m r` to `Stream ((,) a) m r` or the like, we get the standard idea of a producer or generator. If it is instantiated to `Stream f Identity m r` then we have the standard \_free monad construction/. This construction is subject to certain familiar objections from an efficiency perspective; efforts have been made to substitute exotic cps-ed implementations and so forth. It is an interesting topic.

But in fact, the standard alarmist talk about *retraversing binds* and *quadratic explosions* and *costly appends*, and so on become transparent nonsense with `Stream f m r`\
in its streaming use. The conceptual power needed to see this is basically nil: Where `m` is read as `IO`, or some transformed `IO`, then the dreaded *retraversing of the binds* in a stream expression would involve repeating all the past actions. Don't worry, to get e.g. the second chunk of bytes from a handle, you won't need to start over and get the first one again! The first chunk has vanished into an unrepeatable past.

All of the difficulties a streaming library is attempting to avoid are concentrated in the deep irrationality of

    sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)

In the streaming context, this becomes

    sequence :: Monad m, Functor f => Stream f m r -> Stream f m r
    sequence = id

It is of course easy enough to define

    accumulate :: Monad m, Functor f => Stream f m r -> m (Stream f Identity r)

or `reifyBindsRetraversingWherePossible` or `_ICan'tTakeThisStreamingAnymore`, as you might call it. *The types themselves* teach the user how to avoid or control the sort of accumulation characteristic of `sequence` in its various guises e.g. `mapM f = sequence . map f` and `traverse f = sequence . fmap f` and `replicateM n = sequence . replicate n`. See for example the types of

    Control.Monad.replicateM :: Int -> m a -> m [a]
    Streaming.Prelude.replicateM :: Int -> m a -> Stream (Of a) m ()

If you want to tempt fate and replicate the irrationality of `Control.Monad.replicateM`, then sure, you can define the hermaphroditic chimera

    accumulate . Streaming.Prelude.replicateM :: Int -> m a -> m (Stream (Of a) Identity ())

which is what we find in our diseased base libraries. But once you know how to operate with a stream directly you will see less and less point in what is called *extracting the (structured) value from IO*. Consider the apparently innocent distinction between

    "getContents" :: String

and

    getContents :: IO String 

Omitting consideration of eof, we might define `getContents` thus

    getContents = sequence $ repeat getChar

There it is again! The very devil! By contrast there is no distinction between

    "getContents" :: Stream (Of Char) m ()  -- the IsString instance is monad-general

and

    getContents :: MonadIO m => Stream (Of Char) m ()

They unify just fine. That is, if I make the type synonym

    type String m r = Stream (Of Char) m r

I get, for example:

    "getLine"                              :: String m  ()
    getLine                                :: String IO ()
    "getLine" >> getLine                   :: String IO ()
    splitAt 20 $ "getLine" >> getLine      :: String IO (String IO ())
    length $ "getLine" >> getLine          :: IO Int

and can dispense with half the advice they will give you on `#haskell`. It is only a slight exaggeration to say that a stream should never be "extracted from IO". 

With `sequence` and `traverse`, we accumulate a pure succession of pure values from a pure succession of monadic values. Why bother if you have intrinsically monadic conception of succession or traversal? `Stream f m r` gives you an immense body of such structures and a simple discipline for working with them. Spinkle `id` freely though your program, under various names, if you get homesick for `sequence` and company.

§ 7. Interoperation with the streaming-io libraries
-----------------------------------------------------

The simplest form of interoperation with [pipes](http://hackage.haskell.org/package/pipes) is accomplished with this isomorphism:

    Pipes.unfoldr Streaming.next        :: Stream (Of a) m r   -> Producer a m r
    Streaming.unfoldr Pipes.next        :: Producer a m r      -> Stream (Of a) m r                     

Of course, `streaming` can be mixed with `pipes` wherever `pipes` itself employs `Control.Monad.Trans.Free`; speedups are frequently appreciable. (This was the original purpose of the main `Streaming` module, which just mechanically transposes a simple optimization employed in `Pipes.Internal`.) Interoperation with [io-streams](http://hackage.haskell.org/package/io-streams) is thus:

    Streaming.reread IOStreams.read     :: InputStream a       -> Stream (Of a) IO ()
    IOStreams.unfoldM Streaming.uncons  :: Stream (Of a) IO () -> IO (InputStream a)

A simple exit to [conduit](http://hackage.haskell.org/package/conduit) would be, e.g.:

    Conduit.unfoldM Streaming.uncons    :: Stream (Of a) m ()  -> Source m a

These conversions should never be more expensive than a single `>->` or `=$=`.

At a much more general level, we also of course have interoperation with [free](http://hackage.haskell.org/package/free):

    Free.iterTM  Stream.wrap              :: FreeT f m a -> Stream f m a
    Stream.iterTM Free.wrap               :: Stream f m a -> FreeT f m a 

§ 8. Where can I find examples of use?
-----------------------------------------

For some simple ghci examples, see the commentary throughout the Prelude module. For slightly more advanced usage see the commentary in the haddocks of [streaming-bytestring](https://hackage.haskell.org/package/streaming-bytestring) and e.g. [these replicas](https://gist.github.com/michaelt/6c6843e6dd8030e95d58) of shell-like programs from the io-streams tutorial. Here's a simple [streaming GET request](https://gist.github.com/michaelt/2dcea1ba32562c091357) with intrinsically streaming byte streams.  Here is a comically simple ['high - low' game](https://gist.github.com/michaelt/242f6a23267707ad29e9)

§ 9. Problems
---------------

Questions about this library can be put as issues through the github site or on the [pipes mailing list](https://groups.google.com/forum/#!forum/haskell-pipes). (This library understands itself as part of the pipes "ecosystem.")



§ 10. Implementation and benchmarking notes
----------------------------------------------

This library defines an optimized `FreeT` with an eye to use with streaming libraries, namely:

    data Stream f m r
         = Return r
         | Step !(f (Stream f m r))
         | Effect (m (Stream f m r))

in place of the standard `FreeT` that we find in the `free` library, which is approximately:

    newtype FreeT f m r = FreeT {runFreeT :: m (Either r (f (FreeT f m r)))}

Rather than wrapping each step in a monadic 'layer', such a layer is put alongside separate 'pure' constructors for a functor 'layer' and a final return value. The maneuver is very friendly to the compiler, but requires a bit of subtlety to protect a sound monad instance. Just such an optimization is adopted internally by the `pipes` library. As in `pipes`, the constructors are here left in an `Internal` module; the main `Streaming` module exporting the type itself and various operations and instances.

I ran a simple [benchmark](https://gist.github.com/michaelt/7f89dc8b366b30bb6acc) (adjusting a [script](https://github.com/jwiegley/streaming-tests) of John Weigly) using a very simple composition of functions:

    toList 
    . filter (\x -> x `mod` 2 == 0) 
    . map (+1) 
    . drop 1000 
    . map (+1) 
    . filter even 
    . each

as it interpreted by various libraries - `streaming`, `conduit`, (Weigley's) `simple-conduit`, `io-streams` and `machines`.

The the results were fairly pleasing:

    benchmarking basic/stream
    time                 85.45 ms   (81.63 ms .. 89.32 ms)
                         0.994 R²   (0.982 R² .. 0.999 R²)
    mean                 86.53 ms   (84.16 ms .. 90.51 ms)
    std dev              4.987 ms   (2.301 ms .. 7.906 ms)
    variance introduced by outliers: 18% (moderately inflated)

    benchmarking basic/conduit
    time                 101.3 ms   (88.77 ms .. 111.3 ms)
                         0.976 R²   (0.911 R² .. 0.996 R²)
    mean                 95.56 ms   (84.90 ms .. 103.6 ms)
    std dev              13.76 ms   (8.210 ms .. 21.79 ms)
    variance introduced by outliers: 43% (moderately inflated)

    benchmarking basic/simple-conduit
    time                 199.2 ms   (174.1 ms .. 215.6 ms)
                         0.993 R²   (0.978 R² .. 1.000 R²)
    mean                 198.4 ms   (190.0 ms .. 202.2 ms)
    std dev              7.091 ms   (1.565 ms .. 10.000 ms)
    variance introduced by outliers: 14% (moderately inflated)

    benchmarking basic/pipes
    time                 211.7 ms   (180.8 ms .. 232.7 ms)
                         0.991 R²   (0.974 R² .. 1.000 R²)
    mean                 207.7 ms   (199.1 ms .. 218.7 ms)
    std dev              12.34 ms   (5.989 ms .. 17.67 ms)
    variance introduced by outliers: 15% (moderately inflated)

    benchmarking basic/data-list
    time                 202.7 ms   (186.5 ms .. 225.5 ms)
                         0.990 R²   (0.970 R² .. 1.000 R²)
    mean                 199.3 ms   (188.4 ms .. 207.4 ms)
    std dev              11.67 ms   (6.966 ms .. 15.11 ms)
    variance introduced by outliers: 15% (moderately inflated)

    benchmarking basic/iostreams
    time                 265.7 ms   (247.2 ms .. 284.8 ms)
                         0.997 R²   (0.990 R² .. 1.000 R²)
    mean                 265.6 ms   (261.9 ms .. 272.8 ms)
    std dev              7.094 ms   (146.8 μs .. 8.387 ms)
    variance introduced by outliers: 16% (moderately inflated)

    benchmarking basic/machines
    time                 1.123 s    (NaN s .. 1.206 s)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 1.134 s    (1.114 s .. 1.145 s)
    std dev              17.29 ms   (0.0 s .. 19.07 ms)
    variance introduced by outliers: 19% (moderately inflated)



This sequence of pre-packaged combinators is, I think, as friendly as it could possibly be to the more recent conduit fusion framework. That framework of course doesn't apply to user-defined operations; there we should expect times like those shown for pipes. Since the combinators from `streaming` are defined with naive recursion, more or less as the user might, we have reason to think this result is characteristic, but much more benchmarking is needed before anything can be said with certainty.