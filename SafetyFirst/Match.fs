namespace SafetyFirst

[<AutoOpen>]
module Match =
  /// <summary>
  /// Matches a <c>seq</c> that has at least one element.  On a match, returns a tuple with
  /// the head and tail of the sequence.
  /// Note, this will evaluate the first element of the sequence twice.
  /// If it is expensive to produce, it is recommended that you cache the
  /// sequence with <c>Seq.cache</c> (if it's finite).
  /// </summary>
  let (|SeqOneOrMore|_|) xs = 
    Seq.tryHead xs 
    |> Option.map (fun head -> (head, Seq.skip 1 xs))

  /// <summary>
  /// Matches a <c>seq</c> that has at least two elements.  On a match, returns a tuple with
  /// the two heads and the tail of the sequence.
  /// Note, this will evaluate the first two elements of the sequence twice.
  /// If they are expensive to produce, it is recommended that you cache the
  /// sequence with <c>Seq.cache</c> (if it's finite).
  /// </summary>
  let (|SeqTwoOrMore|_|) xs =
    let truncated = Seq.truncate 2 xs |> Seq.toList
    match truncated with
    | [head1; head2] -> Some (head1, head2, Seq.skip 2 xs)
    | _ -> None

  /// <summary>
  /// Matches a <c>seq</c> that has at least three elements.  On a match, returns a tuple with
  /// the three heads and the tail of the sequence.
  /// Note, this will evaluate the first three elements of the sequence twice.
  /// If they are expensive to produce, it is recommended that you cache the
  /// sequence with <c>Seq.cache</c> (if it's finite).
  /// </summary>
  let (|SeqThreeOrMore|_|) xs =
    let truncated = Seq.truncate 3 xs |> Seq.toList
    match truncated with
    | [head1; head2; head3] -> Some (head1, head2, head3, Seq.skip 3 xs)
    | _ -> None

  /// <summary>
  /// Matches a <c>seq</c> that has at least four elements.  On a match, returns a tuple with
  /// the four heads and the tail of the sequence.
  /// Note, this will evaluate the first four elements of the sequence twice.
  /// If they are expensive to produce, it is recommended that you cache the
  /// sequence with <c>Seq.cache</c> (if it's finite).
  /// </summary>
  let (|SeqFourOrMore|_|) xs =
    let truncated = Seq.truncate 4 xs |> Seq.toList
    match truncated with
    | [head1; head2; head3; head4] -> Some (head1, head2, head3, head4, Seq.skip 4 xs)
    | _ -> None

  /// <summary>
  /// Matches a <c>seq</c> that has at least five elements.  On a match, returns a tuple with
  /// the five heads and the tail of the sequence.
  /// Note, this will evaluate the first five elements of the sequence twice.
  /// If they are expensive to produce, it is recommended that you cache the
  /// sequence with <c>Seq.cache</c> (if it's finite).
  /// </summary>
  let (|SeqFiveOrMore|_|) xs =
    let truncated = Seq.truncate 5 xs |> Seq.toList
    match truncated with
    | [head1; head2; head3; head4; head5] -> Some (head1, head2, head3, head4, head5, Seq.skip 5 xs)
    | _ -> None
