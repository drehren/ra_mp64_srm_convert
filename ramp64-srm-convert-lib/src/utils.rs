/// Swaps value in a 4-byte boundary
pub(crate) fn word_swap(buf: &mut [u8]) {
  for i in (0..buf.len()).step_by(4) {
    buf.swap(i, i + 3);
    buf.swap(i + 1, i + 2);
  }
}

#[cfg(test)]
mod tests {
  use super::word_swap;

  #[test]
  fn verify_word_swap() {
    let mut bytes = [1, 2, 3, 4];
    word_swap(&mut bytes);
    assert_eq!(bytes, [4, 3, 2, 1]);
  }
}
