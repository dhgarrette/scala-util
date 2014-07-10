package dhg.util

import java.io.Closeable
import scala.io.Source
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversableLike
import scala.collection.Parallel
import scala.collection.GenTraversable

package object `packagex` {

  // Arm
  //TODO: def using[T, R](resource: dhg.util.Arm.Managed[T])(block: T => R) = dhg.util.Arm.using[T, R](resource)(block)
  //TODO: type ManagedCloseable[T <: Closeable] = dhg.util.Arm.ManagedCloseable[T]
  //TODO: type ManagedSource[T <: Source] = dhg.util.Arm.ManagedSource[T]

  // Collections
  type UniversalSet[A] = dhg.util.Collections.UniversalSet[A]
  type WindowIteratorish[A] = dhg.util.Collections.WindowIteratorish[A]
  type NextWhileIteratorish[A] = dhg.util.Collections.NextWhileIteratorish[A]
  type MemoMap[A, B] = dhg.util.Collections.MemoMap[A, B]
  //TODO: type History[A] = dhg.util.Collections.History[A]

  // CollectionUtil
  type Enriched_toTuple_Seq[A] = dhg.util.CollectionUtil.Enriched_toTuple_Seq[A]
  type Enriched_toTuple_Array[A] = dhg.util.CollectionUtil.Enriched_toTuple_Array[A]
  type Enriched_prependAppend_Iterator[A] = dhg.util.CollectionUtil.Enriched_prependAppend_Iterator[A]
  type Enriched_counts_TraversableOnce[A] = dhg.util.CollectionUtil.Enriched_counts_TraversableOnce[A]
  type Enriched_groupBy_Iterator[A] = dhg.util.CollectionUtil.Enriched_groupBy_Iterator[A]
  type Enriched_groupByKey_Traversable[K, V, Repr] = dhg.util.CollectionUtil.Enriched_groupByKey_Traversable[K, V, Repr]
  type Enriched_groupByKey_Iterator[A] = dhg.util.CollectionUtil.Enriched_groupByKey_Iterator[A]
  type Enriched_ungroup_GenTraversableOnce[A, B] = dhg.util.CollectionUtil.Enriched_ungroup_GenTraversableOnce[A, B]
  type Enriched_dropRightWhile_Seq[A, Repr] = dhg.util.CollectionUtil.Enriched_dropRightWhile_Seq[A, Repr]
  type Enriched_dropRightWhile_String = dhg.util.CollectionUtil.Enriched_dropRightWhile_String
  type Enriched_splitAt_Iterator[A] = dhg.util.CollectionUtil.Enriched_splitAt_Iterator[A]
  type KeepDelimiter = dhg.util.CollectionUtil.KeepDelimiter
  // TODO: type DropDelimiter = dhg.util.CollectionUtil.KeepDelimiter.DropDelimiter
  // TODO: type KeepDelimiterAsFirst = dhg.util.CollectionUtil.KeepDelimiter.KeepDelimiterAsFirst
  // TODO: type KeepDelimiterAsLast = dhg.util.CollectionUtil.KeepDelimiter.KeepDelimiterAsLast
  type Enriched_split_Iterator[A] = dhg.util.CollectionUtil.Enriched_split_Iterator[A]
  type Enriched_split_Traversable[A, Repr] = dhg.util.CollectionUtil.Enriched_split_Traversable[A, Repr]
  type Enriched_splitWhere_Iterator[A] = dhg.util.CollectionUtil.Enriched_splitWhere_Iterator[A]
  type Enriched_splitWhere_Traversable[A, That] = dhg.util.CollectionUtil.Enriched_splitWhere_Traversable[A, That]
  type Enriched_zipSafe_Iterator[A] = dhg.util.CollectionUtil.Enriched_zipSafe_Iterator[A]
  type Enriched_zipSafe_GenTraversable[A, Repr] = dhg.util.CollectionUtil.Enriched_zipSafe_GenTraversable[A, Repr]
  type Enriched_zipSafe_Tuple_of_Iterator[A, B] = dhg.util.CollectionUtil.Enriched_zipSafe_Tuple_of_Iterator[A, B]
  type Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B] = dhg.util.CollectionUtil.Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B]
  def zipSafe[A, Repr, B, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = zipSafe[A, Repr, B, A1, That](a, b)(bf)
  // TODO: zipSafe 3
  // TODO: zipSafe 4
  type Enriched_unzip2_Iterator[A, B] = dhg.util.CollectionUtil.Enriched_unzip2_Iterator[A, B]
  type Enriched_mapTo_GenTraversableLike[A, Repr] = dhg.util.CollectionUtil.Enriched_mapTo_GenTraversableLike[A, Repr]
  type Enriched_mapTo_Iterator[A] = dhg.util.CollectionUtil.Enriched_mapTo_Iterator[A]
  type Enriched_mapToVal_GenTraversableLike[A, Repr] = dhg.util.CollectionUtil.Enriched_mapToVal_GenTraversableLike[A, Repr]
  type Enriched_mapToVal_Iterator[A] = dhg.util.CollectionUtil.Enriched_mapToVal_Iterator[A]
  type Enriched_mapKeys_GenTraversable[T, U, Repr] = dhg.util.CollectionUtil.Enriched_mapKeys_GenTraversable[T, U, Repr]
  type Enriched_mapKeys_Iterator[T, U] = dhg.util.CollectionUtil.Enriched_mapKeys_Iterator[T, U]
  type Enriched_mapVals_GenTraversable[T, U, Repr] = dhg.util.CollectionUtil.Enriched_mapVals_GenTraversable[T, U, Repr]
  type Enriched_mapVals_Iterator[T, U] = dhg.util.CollectionUtil.Enriched_mapVals_Iterator[T, U]
  type Enriched_submap_GenTraversable[T, TRepr, SRepr] = dhg.util.CollectionUtil.Enriched_submap_GenTraversable[T, TRepr, SRepr]
  type Enriched_submap_Iterator[T, Repr] = dhg.util.CollectionUtil.Enriched_submap_Iterator[T, Repr]
  type Enriched_mapt_2_GenTraversableLike[A, B, Repr] = dhg.util.CollectionUtil.Enriched_mapt_2_GenTraversableLike[A, B, Repr]
  type Enriched_mapt_2_Iterator[A, B] = dhg.util.CollectionUtil.Enriched_mapt_2_Iterator[A, B]
  // TODO: mapt 3
  // TODO: mapt 3
  // TODO: mapt 4
  // TODO: mapt 4
  type Enriched_foldLeftWhile_GenTraversableOnce[A] = dhg.util.CollectionUtil.Enriched_foldLeftWhile_GenTraversableOnce[A]
  type Enrich_avg_GenTraversableOnce[A] = dhg.util.CollectionUtil.Enrich_avg_GenTraversableOnce[A]
  type Enrich_avg_Int_GenTraversableOnce = dhg.util.CollectionUtil.Enrich_avg_Int_GenTraversableOnce
  type Enriched_normalize_GenTraversable[A, Repr] = dhg.util.CollectionUtil.Enriched_normalize_GenTraversable[A, Repr]
  type Enriched_normalize_Int_GenTraversable[Repr] = dhg.util.CollectionUtil.Enriched_normalize_Int_GenTraversable[Repr]
  type Enriched_normalizeValues_GenTraversable[T, U, Repr] = dhg.util.CollectionUtil.Enriched_normalizeValues_GenTraversable[T, U, Repr]
  type Enriched_normalizeValues_Int_GenTraversable[T, Repr] = dhg.util.CollectionUtil.Enriched_normalizeValues_Int_GenTraversable[T, Repr]
  type Enriched_maxByN_GenTraversable[A, Repr] = dhg.util.CollectionUtil.Enriched_maxByN_GenTraversable[A, Repr]
  type Enriched_maxByN_Iterator[A] = dhg.util.CollectionUtil.Enriched_maxByN_Iterator[A]
  type Enriched_minByN_GenTraversable[A, Repr] = dhg.util.CollectionUtil.Enriched_minByN_GenTraversable[A, Repr]
  type Enriched_minByN_Iterator[A] = dhg.util.CollectionUtil.Enriched_minByN_Iterator[A]
  type Enriched_sumBy_GenTraversableOnce[A] = dhg.util.CollectionUtil.Enriched_sumBy_GenTraversableOnce[A]
  type Enriched_slidingN_Iterator[A] = dhg.util.CollectionUtil.Enriched_slidingN_Iterator[A]
  type Enriched_slidingN_GenTraversableLike[A, Repr <: GenTraversable[A]] = dhg.util.CollectionUtil.Enriched_slidingN_GenTraversableLike[A, Repr]
  type Enriched_slyce_GenTraversable[A, Repr <: GenTraversable[A]] = dhg.util.CollectionUtil.Enriched_slyce_GenTraversable[A, Repr]
  type Enriched_countCompare_GenTraversableOnce[A] = dhg.util.CollectionUtil.Enriched_countCompare_GenTraversableOnce[A]
  type Enriched_takeSub_Iterator[A, R <: GenTraversable[A]] = dhg.util.CollectionUtil.Enriched_takeSub_Iterator[A, R]
  type Enriched_takeSub_GenTraversableLike[A, R <: GenTraversable[A], Repr <: GenTraversable[GenTraversable[A]]] = dhg.util.CollectionUtil.Enriched_takeSub_GenTraversableLike[A, R, Repr]
  type Enriched_AscDesc_GenTraversableOnce[K, V] = dhg.util.CollectionUtil.Enriched_AscDesc_GenTraversableOnce[K, V]
  type Enriched_last_Iterator[A] = dhg.util.CollectionUtil.Enriched_last_Iterator[A]
  type Enriched_noop_GenTraversableLike[A, Repr] = dhg.util.CollectionUtil.Enriched_noop_GenTraversableLike[A, Repr]
  type Enriched_noop_Iterator[A] = dhg.util.CollectionUtil.Enriched_noop_Iterator[A]
  type Enriched_shuffle_Seq[A, Repr] = dhg.util.CollectionUtil.Enriched_shuffle_Seq[A, Repr]
  type Enriched_shuffle_Iterator[A] = dhg.util.CollectionUtil.Enriched_shuffle_Iterator[A]
  type Enriched_toBitSet_GenTraversableOnce = dhg.util.CollectionUtil.Enriched_toBitSet_GenTraversableOnce
  type Enriched_updateWith_MutableMap[K, V] = dhg.util.CollectionUtil.Enriched_updateWith_MutableMap[K, V]
  type Enriched_PARALLEL_Parallelizable[+A, +ParRepr <: Parallel] = dhg.util.CollectionUtil.Enriched_PARALLEL_Parallelizable[A, ParRepr]
  type Enriched_SEQUENTIAL_Iterator[+A] = dhg.util.CollectionUtil.Enriched_SEQUENTIAL_Iterator[A]
  type Enriched_SEQUENTIAL_GenTraversableLike[+A, Repr] = dhg.util.CollectionUtil.Enriched_SEQUENTIAL_GenTraversableLike[A, Repr]

}
