package de.tu_dresden.inf.lat.concurrent;

public class ConcurrentBoundedBitSet2 {

//    public static final java.lang.invoke.VarHandle varHandleArray;
//
//    static {
//        try {
//            varHandleArray = java.lang.invoke.MethodHandles.lookup().findVarHandle(ConcurrentBitSetJava.class, "array", long[].class);
//        } catch (ReflectiveOperationException e) {
//            throw new ExceptionInInitializerError(e);
//        }
//    }

    public static final java.lang.invoke.VarHandle handle = java.lang.invoke.MethodHandles.arrayElementVarHandle(long[].class);

    protected final long[] array;
    private final int maxElem;

    public ConcurrentBoundedBitSet2(int maxElem) {
        this.array = new long[(maxElem >> 6) + 1];
        this.maxElem = maxElem;
    }

    public ConcurrentBoundedBitSet2(long[] elems) {
        this.array = elems;
        this.maxElem = ((elems.length << 6) - 1);
    }

    public scala.collection.immutable.BitSet viewAsImmutableBitSet() {
        return scala.collection.immutable.BitSet$.MODULE$.fromBitMaskNoCopy(array);
    }

    public scala.collection.mutable.BitSet viewAsMutableBitSet() {
        return scala.collection.mutable.BitSet$.MODULE$.fromBitMaskNoCopy(array);
    }

    public scala.collection.immutable.BitSet copyToImmutableBitSet() {
        return scala.collection.immutable.BitSet$.MODULE$.fromBitMask(array);
    }

    public scala.collection.mutable.BitSet copyToMutableBitSet() {
        return scala.collection.mutable.BitSet$.MODULE$.fromBitMask(array);
    }

    public void add(int elem) {
        scala.Predef.require((0 <= elem) && (elem <= maxElem));
        handle.getAndBitwiseOr(array, elem >> 6, 1L << elem);
    }

    public void remove(int elem) {
        scala.Predef.require((0 <= elem) && (elem <= maxElem));
        handle.getAndBitwiseAnd(array, elem >> 6, ~(1L << elem));
    }

    public boolean contains(int elem) {
        return (0 <= elem) && (elem <= maxElem) && ((long) handle.getVolatile(array, elem >> 6) & (1L << elem)) != 0L;
    }

}
