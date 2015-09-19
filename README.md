streaming
=========

The freely-extended stream on a streamable functor
---------------------------------------------------

`Stream` can be used wherever [FreeT](https://hackage.haskell.org/package/free-4.12.1/docs/Control-Monad-Trans-Free.html) is used. The compiler's standard range of optimizations work better for operations written in terms of `Stream`. `FreeT f m r` and `Stream f m r` are of course extremely general, and many functor-general combinators are exported by the general module `Streaming`.

The general idea of streaming
-----------------------------

As soon as you consider the idea of an effectful stream of any kind whatsoever, for example, a stream of bytes from a handle, however constituted, you will inevitably be forced to contemplate the idea of a streaming *succession* of *such streams*. Thus, for example, however you imagine your bytes streaming from a handle, you will want to consider a *succession* of *such streams* divided on newlines. Similarly, suppose you have the idea the unfolding of some sort of stream from a Haskell value, a seed - a file name, as it might be. And suppose you *also* have some idea of a stream of such Haskell values - maybe a stream of file names coming from something like `du`, subjected to some filter. Then you will also have the idea of a streaming *succession* of *such unfoldings* linked together end to end in accordance with the initial succession of seed values.

Call those 5 sentences the ABCs of streaming. If you understood these ABCs you have a total comprehension of `Stream f m r`:

-   `Stream` itself expresses what the word "succession" meant in the ABCs
-   The general parameter `f` expresses what was meant by "such streams"
-   `m` expresses the relevant form of "effect".

General combinators for working with this idea of succession irrespective of the form of succession are contained in the module `Stream`. They can be used, or example, to organize a succession of io-streams `Generator`s or pipes `Producer`s or the effectful bytestreams of the [streaming-bytestring](https://hackage.haskell.org/package/streaming-bytestring) library, or whatever stream-form you can express in a Haskell functor.

A freely generated stream of connected individual Haskell values is a Producer, Generator or Source
---------------------------------------------------------------------------------------------------

But, of course, as soon as you grasp the general form of *succession/, you are already in possession of the most basic concrete form: a simple *succession of individual Haskell values\_ one after another. This is just `Stream ((,) a) m r`, or as we write it here, `Stream (Of a) m r`, strictifying the left element of the pair. The pairing just links the present element with the rest of the stream. The primitive `yield` statement just expresses the pairing of the yielded item with the rest of the stream; or rather it is itself the trivial singleton stream. `Streaming.Prelude` is focused on the manipulation of this all-important stream-form, which appears in the streaming IO libraries under titles like:

    io-streams: Generator a r
    pipes:      Producer a m r
    conduit:    ConduitM () o m r
    streaming:  Stream (Of a) m r

The only difference is that in `streaming` the simple Generator or Producer concept is formulated explicitly in terms of the *general* concept of successive connection. But this is a concept you need and already possess anyway, as your comprehension of the four sentences above showed.

The special case of a *stream of individual Haskell values* that simply *comes to an end without a special result* is variously expressed thus:

    io-streams: InputStream a 
    pipes:      Producer a m ()
    conduit:    Source m a
    machines:   SourceT m a (= forall k. MachineT m k a)
    streaming:  Stream (Of a) m ()

`Streaming.Prelude`
-------------------

`Streaming.Prelude` closely follows `Pipes.Prelude`. But since it restricts itself to use only of the general idea of streaming, it cleverly \_omits the pipes/:

    ghci> S.stdoutLn $ S.take 2 S.stdinLn
    let's<Enter>
    let's
    stream<Enter>
    stream

Here's a little \_connect and resume/, as the streaming-io experts call it:

    ghci> rest <- S.print $ S.splitAt 3 $ S.each [1..10]
    1
    2
    3
    ghci> S.sum rest
    49

Somehow, we didn't even need a four-character operator for that, nor advice about best practices! - just ordinary Haskell common sense.

Mother's `Prelude` v. `Streaming.Prelude`
-----------------------------------------

The effort of `Streaming.Prelude` is to leverage the intuition the user has acquired in mastering `Prelude` and `Data.List` and to elevate her understanding into a general comprehension of effectful streaming transformations. Unsurprisingly, it takes longer to type out the signatures. It cannot be emphasized enough, thought, that *the transpositions are totally mechanical*:

    Data.List.Split.chunksOf :: Int -> [a]          -> [[a]]
    Streaming.chunksOf       :: Int -> Stream f m r -> Stream (Stream f m) m r

    Prelude.splitAt   :: Int -> [a]          -> ([a],[a])
    Streaming.splitAt :: Int -> Stream f m r -> Stream f m (Stream f m r)

These concepts are "functor general", in the jargon used in the documentation, and are thus exported by the main `Streaming` module. Something like `break` requires us to inspect individual values for their properties, so it is found in the `Streaming.Prelude`

    Prelude.break           :: (a -> Bool) -> [a]               -> ([a],[a])
    Streaming.Prelude.break :: (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m (Stream (Of a) m r)

It is easy to prove that \_resistance to these types is resistance to effectful streaming itself/. I will labor this point a bit more below, but you can also find it developed, with greater skill, in the documentation for the pipes libraries.

How come there's not one of those fancy "ListT done right" implementations in here?
-----------------------------------------------------------------------------------

The use of the final return value appears to be a complication, but in fact it is essentially contained in the idea of effectful streaming. This is why this library does not export a \_ListT done right/, which would be simple enough - following `pipes`, as usual:

    newtype ListT m a = ListT (Stream (Of a) m ())

The associated monad instance would wrap

    yield :: (Monad m)            => a -> Stream (Of a) m ()
    for   :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m ()) -> Stream f m r

To see the trouble, consider [this signature](http://hackage.haskell.org/package/list-t-0.4.5.1/docs/ListT.html#v:splitAt) for splitting a ListT very much done right. Here's what becomes of [chunksOf](http://hackage.haskell.org/package/list-t-0.4.5.1/docs/src/ListT.html#slice). As long as we are trapped in some sort of ListT, however much rightly implemented, these operations can't be made to stream; something like a list must be accumulated. Similarly, try to imagine adding a `splitAt` or `lines` function to [this API](https://hackage.haskell.org/package/list-t-text-0.2.0.2/docs/ListT-Text.html). It would accumulate strict text forever, just as [this does](https://hackage.haskell.org/package/io-streams-1.3.2.0/docs/System-IO-Streams-ByteString.html#v:lines) and [this doesn't](https://hackage.haskell.org/package/pipes-bytestring-2.1.1/docs/src/Pipes-ByteString.html#lines) and [this doesn't](https://hackage.haskell.org/package/streaming-bytestring-0.1.0.6/docs/Data-ByteString-Streaming-Char8.html#v:lines) The difference is simply that the latter libraries operate with the general concept of streaming, and the whole implementation is governed by it. The attractions of the various "`ListT` done right" implementations are superficial; the concept belongs to logic programming, not stream programming.

Note similarly that you can write a certain kind of [take](http://hackage.haskell.org/package/machines-0.5.1/docs/Data-Machine-Process.html#v:taking) and [drop](http://hackage.haskell.org/package/machines-0.5.1/docs/Data-Machine-Process.html#v:dropping) with the `machines` library - as you can even with a "`ListT` done right". But I wish you luck writing `splitAt`! Similarly you can write a [getContents](http://hackage.haskell.org/package/machines-io-0.2.0.6/docs/System-IO-Machine.html); but I wish you luck dividing the resulting bytestream on its lines. This is - as usual! - because the library was not written with the general concept of effectful succession or streaming in view. Materials for sinking some elements of a stream in one way, and others in other ways - copying each line to a different file, as it might be, but without accumulation - are documented within. So are are myriad other elementary operations of streaming io.

Didn't I hear that free monads are a dog from the point of view of efficiency?
------------------------------------------------------------------------------

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

which is what we find in our diseased base libraries. But once you know how to operate with a stream directly you will see less and less point in what is called *extracting the (structured) value from IO*. The distinction between

    "getContents" :: String

and

    getContents :: IO String 

but, omitting consideration of eof, we might define `getContents` thus

    getContents = sequence $ repeat getChar

There it is again! The very devil! By contrast there is no distinction between

    "getContents" :: Stream (Of Char) m ()

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

With `sequence` and `traverse`, we accumulate a pure succession of pure values from a pure succession of monadic values.\
Why bother if you have intrinsically monadic conception of succession or traversal? `Stream f m r` gives you an immense body of such structures and a simple discipline for working with them. Spinkle `id` freely though your program if you get homesick.

Interoperation with the streaming-io libraries
----------------------------------------------

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

Where can I find examples of use?
---------------------------------

For some simple ghci examples, see the commentary throughout the Prelude module. For slightly more advanced usage see the commentary in the haddocks of [streaming-bytestring](https://hackage.haskell.org/package/streaming-bytestring) and e.g. [these replicas](https://gist.github.com/michaelt/6c6843e6dd8030e95d58) of shell-like programs from the io-streams tutorial. Here's a simple [streaming GET request](https://gist.github.com/michaelt/2dcea1ba32562c091357) with intrinsically streaming byte streams.

Problems
--------

Questions about this library can be put as issues through the github site or on the [pipes mailing list](https://groups.google.com/forum/#!forum/haskell-pipes). (This library understands itself as part of the pipes "ecosystem.")

* * * * *

implementation notes

This library defines an optimized `FreeT` with an eye to use with streaming libraries, namely:

    data Stream f m r
         = Return r
         | Step !(f (Stream f m r))
         | Delay (m (Stream f m r))

in place of the standard `FreeT` that we find in the `free` library, which is approximately:

    newtype FreeT f m r = FreeT {runFreeT :: m (Either r (f (FreeT f m r)))}

Rather than wrapping each step in a monadic 'layer', such a layer is put alongside separate 'pure' constructors for a functor 'layer' and a final return value. The maneuver is very friendly to the compiler, but requires a bit of subtlety to protect a sound monad instance. Just such an optimization is adopted internally by the `pipes` library. As in `pipes`, the constructors are here left in an `Internal` module; the main `Streaming` module exporting the type itself and various operations and instances.

There is also a still-incomplete `Prelude` of functions, some `FreeT` or `Stream` - general, some involving the functor `((,) a)` here called `Of a`. (`Stream (Of a) m r` like `FreeT ((,) a) m r` is equivalent to the `pipes` `Producer a m r` type. Similarly, `Stream (Of a) m ()` and `FreeT ((,) a) m ()` are possible implementations of `ListT done right`.

I ran a simple [benchmark](https://gist.github.com/michaelt/7f89dc8b366b30bb6acc) (adjusting a [script](https://github.com/jwiegley/streaming-tests) of John Weigly) using a very simple composition of functions:

    toList 
    . filter (\x -> x `mod` 2 == 0) 
    . map (+1) 
    . drop 1000 
    . map (+1) 
    . filter even 
    . each

The the results were fairly pleasing:

    benchmarking basic/streaming
    time                 84.50 ms   (79.81 ms .. 87.90 ms)

    benchmarking basic/iostreams
    time                 266.2 ms   (235.6 ms .. 292.0 ms)

    benchmarking basic/pipes
    time                 232.0 ms   (206.6 ms .. 246.7 ms)

    benchmarking basic/conduit
    time                 102.3 ms   (96.24 ms .. 110.0 ms)

This sequence of pre-packaged combinators is, I think, very friendly to the more recent conduit fusion framework. The framework of course doesn't apply to user-defined operations, where we should expect times like those shown for pipes. Since the combinators from `streaming` is defined with naive recursion, more or less as the user might, we have reason to think the result is characteristic, but much more benchmarking is needed before anything can be said with certainty. The labor of constructor-hiding may turn up some further difficulty.
