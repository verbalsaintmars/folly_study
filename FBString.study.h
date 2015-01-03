/*
 *  type object assigned for loop
 */
template <class InIt, class OutIt>
inline
OutIt copy_n(InIt b,
             typename std::iterator_traits<InIt>::difference_type n, // extract trait type
             OutIt d) {
  for (; n != 0; --n, ++b, ++d) {
    *d = *b;
  }
  return d;
}


/*
 * T is the char_type of a specified type
 *
 *  -- b --
 *  -- e --
 *  e as the end pivot of b
 */
template <class Pod, class T>
inline void pod_fill(Pod* b, Pod* e, T c) {
  assert(b && e && b <= e); // test b/e not nullptr, e has larger mem posistion then b
  if (sizeof(T) == 1) { /* if size of type T is 1 byte, we do memset init.*/
    memset(b, c, e - b); // memset(ptr to void, copy c into e-b bytes of b
  } else {  // else
    /*
     *  ~7u =>
     *  00000111 => 11111000 then & with e-b
     *  i.e like Duff device , we loop by 8x
     *  loop unwinding
     */
    auto const ee = b + ((e - b) & ~7u);
    for (; b != ee; b += 8) {
      b[0] = c;
      b[1] = c;
      b[2] = c;
      b[3] = c;
      b[4] = c;
      b[5] = c;
      b[6] = c;
      b[7] = c;
    }
    // Leftovers , b hasn't hit the pivot yet, thus leftovers
    for (; b != e; ++b) {
      *b = c;
    }
  }
}




/*
 * Lightly structured memcpy, simplifies copying PODs and introduces
 * some asserts. Unfortunately using this function may cause
 * measurable overhead (presumably because it adjusts from a begin/end
 * convention to a pointer/size convention, so it does some extra
 * arithmetic even though the caller might have done the inverse
 * adaptation outside).
 */
template <class Pod>
inline void pod_copy(const Pod* b, const Pod* e, Pod* d) {
    /*
     *  all these testing here make sure d won't be copied overlap with b itself
     */
  assert(e >= b); // e as end pivot, thus must greater then b
  assert(d >= e || d + (e - b) <= b); // d is either behind b or before b
  /*
   * copy b into d with size (e-b) * sizeof(Pod)
   */
  memcpy(d, b, (e - b) * sizeof(Pod)); // distance between e and b then times size Pod
}

/*
 * Lightly structured memmove, simplifies copying PODs and introduces
 * some asserts
 */
template <class Pod>
inline void pod_move(const Pod* b, const Pod* e, Pod* d) {
  assert(e >= b);
  memmove(d, b, (e - b) * sizeof(*b));
}


/**
 * Defines a special acquisition method for constructing fbstring
 * objects. AcquireMallocatedString means that the user passes a
 * pointer to a malloc-allocated string that the fbstring object will
 * take into custody.
 * enum tag usage :-)
 */
enum class AcquireMallocatedString {};

// -- Below inside template class fbstring_core
// template <class Char> class fbstring_core

/**
 * This is the core of the string. The code should work on 32- and
 * 64-bit architectures and with any Char size. Porting to big endian
 * architectures would require some changes.
 *
 * The storage is selected as follows (assuming we store one-byte
 * characters on a 64-bit machine): (a) "small" strings between 0 and
 * 23 chars are stored in-situ without allocation (the rightmost byte
 * stores the size); (b) "medium" strings from 24 through 254 chars
 * are stored in malloc-allocated memory that is copied eagerly; (c)
 * "large" strings of 255 chars and above are stored in a similar
 * structure as medium arrays, except that the string is
 * reference-counted and copied lazily. the reference count is
 * allocated right before the character array.
 *
 * 1. small : 0 ~ 23 chars / on stack / rightmost byte store size
 * 2. medium : 24 ~ 254 chars / on heap / copy always
 * 3. large : 255 ~ chars / on heap / ref counted
 *    ref count is allocated before char array. Details below.
 *
 *  little endian memory layout:
 *  xx000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
 *  LSByte                                                         MSByte
 *  [7]      [6]      [5]      [4]      [3]      [2]      [1]      [0]
 *  xx is the MSBit used for discriminate string category
 *  00 => unset, small string
 *  10 => medium string
 *  01 => large string
 *
 * The discriminator between these three strategies sits in the two
 * most significant bits of the rightmost char of the storage.
 * If
 * neither is set, then the string is small (and its length sits in
 * the lower-order bits of that rightmost character). If the MSb is
 * set, the string is medium width. If the second MSb is set, then the
 * string is large.
 */

/*
 *  Main structure!
 *  size : 24 bytes
 *      8 byte ptr
 *      8 byte unsigned int
 *      8 byte unsigned int
 */
struct MediumLarge {
    Char * data_;
    size_t size_;
    size_t capacity_;

    size_t capacity() const {
      return capacity_ & capacityExtractMask;
    }
};

union {
Char small_[sizeof(MediumLarge) / sizeof(Char)]; // if Char is 1 byte, then small_ is array of 24 * 1
MediumLarge ml_;
};

enum {
    lastChar = sizeof(MediumLarge) - 1, // 24 - 1 = 23
    maxSmallSize = lastChar / sizeof(Char),  // 23 / Char_Size = max small size
    maxMediumSize = 254 / sizeof(Char),      // coincides with the small // 254 bytes
                                             // bin size in dlmalloc
    /*
     * if x86, mask LSByte of 4 bytes
     * if x86_64, mask LSByte of 8 bytes
     */
                                                             // 1100 0000 = 0xC0
    categoryExtractMask = sizeof(size_t) == 4 ? 0xC0000000 : 0xC000000000000000,
    /*
     * Extract size of small string. Max = 63
     */
    capacityExtractMask = ~categoryExtractMask, // 0011 1111 * all 1's 7 bytes
};

// default constructor
fbstring_core() noexcept {
    /*
     * ml_ is type of MediumLarge, 24 bytes for x86_64
     *
     * maxSmallSize = 23
     * ml_.capacity becomes:
     *  little endian memory layout:
     *  00010111 00000000 00000000 00000000 00000000 00000000 00000000 00000000
     *  dec   23
     *  LSByte                                                         MSByte
     *  [7]      [6]      [5]      [4]      [3]      [2]      [1]      [0]
     */
    ml_.capacity_ = maxSmallSize << (8 * (sizeof(size_t) - sizeof(Char)));
    // or: setSmallSize(0);
    writeTerminator();
    assert(category() == Category::isSmall && size() == 0);
}


typedef std::conditional<sizeof(size_t) == 4, uint32_t, uint64_t>::type
      category_type;

enum class Category : category_type { // 8 byte, unsigned int
    isSmall = 0,
    isMedium = sizeof(size_t) == 4 ? 0x80000000 : 0x8000000000000000, // 10 000000
    isLarge =  sizeof(size_t) == 4 ? 0x40000000 : 0x4000000000000000, // 01 000000
};


Category category() const {
    // Assumes little endian
    return static_cast<Category>(ml_.capacity_ & categoryExtractMask);
}


size_t smallSize() const {
    assert(category() == Category::isSmall &&
           static_cast<size_t>(small_[maxSmallSize]) // i.e ml_.capacity's LSByte
           <= static_cast<size_t>(maxSmallSize/*23*/));

    return static_cast<size_t>(maxSmallSize) // 23
      - static_cast<size_t>(small_[maxSmallSize]); // 23 - 23 = 0 for init.
}

void writeTerminator() {
    if (category() == Category::isSmall) {
      const auto s = smallSize(); // 23 - 23 = 0 for init. else, 23 - 20(str size) = 3
      if (s != maxSmallSize) {
        small_[s] = '\0'; // little endian, str char from LSByte to MSByte
      }
    } else {
      ml_.data_[ml_.size_] = '\0';
    }
}


/*
 * Inner structure
 */
struct RefCounted {
    std::atomic<size_t> refCount_;
    Char data_[1];

    static RefCounted * fromData(Char * p) {
      /*
       * Since Char * p's address, before address p,
       * there's RefCounted type size.
       * Thus, minus the sizeof(RefCounted) from p, then
       * cast to type RefCounted.
       */
        /*
         *  This is awesome. sizeof won't evaluate refCount_.
         *  Thus can use it even it's a instance variable.
         */
      return static_cast<RefCounted*>(
        static_cast<void*>(
          static_cast<unsigned char*>(static_cast<void*>(p))
          - sizeof(refCount_)));
    }

    static size_t refs(Char * p) {
      return fromData(p)->refCount_.load(std::memory_order_acquire);
    }

    static void incrementRefs(Char * p) {
      fromData(p)->refCount_.fetch_add(1, std::memory_order_acq_rel);
    }

    static void decrementRefs(Char * p) {
      auto const dis = fromData(p);
      size_t oldcnt = dis->refCount_.fetch_sub(1, std::memory_order_acq_rel);
      assert(oldcnt > 0);
      if (oldcnt == 1) {
        free(dis);
      }
    }

    /*
     * Entry point.
     */
    static RefCounted * create(size_t * size) {
      // Don't forget to allocate one extra Char for the terminating
      // null. In this case, however, one Char is already part of the
      // struct.
      /*
       *  sizeof RefCounted + size *  sizeof(Char)
       */
      const size_t allocSize = goodMallocSize(
        sizeof(RefCounted) + *size * sizeof(Char));
      auto result = static_cast<RefCounted*>(checkedMalloc(allocSize));

      /*
       *  wmb : while got invalid queue ack, fluch store buffer to cache
       *  it's a operation, not fence.
       *  operation only needs to prevent preceding memory operations from being
       *  reordered past itself.
       */
      result->refCount_.store(1, std::memory_order_release);
      /*
       * save back the real size of allocated size
       */
      *size = (allocSize - sizeof(RefCounted)) / sizeof(Char);
      return result;
    }

    static RefCounted * create(const Char * data, size_t * size) {
      const size_t effectiveSize = *size;
      auto result = create(size);
      fbstring_detail::pod_copy(data, data + effectiveSize, result->data_);
      return result;
    }

    static RefCounted * reallocate(Char *const data,
                                   const size_t currentSize,
                                   const size_t currentCapacity,
                                   const size_t newCapacity) {
      assert(newCapacity > 0 && newCapacity > currentSize);
      auto const dis = fromData(data);
      assert(dis->refCount_.load(std::memory_order_acquire) == 1);
      // Don't forget to allocate one extra Char for the terminating
      // null. In this case, however, one Char is already part of the
      // struct.
      auto result = static_cast<RefCounted*>(
             smartRealloc(dis,
                          sizeof(RefCounted) + currentSize * sizeof(Char),
                          sizeof(RefCounted) + currentCapacity * sizeof(Char),
                          sizeof(RefCounted) + newCapacity * sizeof(Char)));
      assert(result->refCount_.load(std::memory_order_acquire) == 1);
      return result;
    }
};




/*
 * SCOPE_EXIT , use lambda trick!
 * ScopeGuard.h
 */

enum class ScopeGuardOnExit {};

/*
 * 1. use std::decay to remove the reference out of type FunctionType
 * 2. operator+ is used as transform purpose.
 * 3. ScopeGuardOnExit enum class is used as a tag feed to operator+
 *    use enum because it's compile time replacement, even none replacement happens...
 */
template <typename FunctionType>
ScopeGuardImpl<typename std::decay<FunctionType>::type>
operator+(detail::ScopeGuardOnExit, FunctionType&& fn) {
  return ScopeGuardImpl<typename std::decay<FunctionType>::type>(
      std::forward<FunctionType>(fn));
}

#define SCOPE_EXIT \
  auto FB_ANONYMOUS_VARIABLE(SCOPE_EXIT_STATE) \
  = ::folly::detail::ScopeGuardOnExit() + [&]() noexcept

#ifndef NDEBUG
#ifndef _LIBSTDCXX_FBSTRING
    SCOPE_EXIT {
      assert(this->size() == size);
      assert(memcmp(this->data(), data, size * sizeof(Char)) == 0);
    };
#endif
#endif


