use nom::{bytes, error::ParseError, Compare, IResult, InputTake};

fn new_signature<I, Error: ParseError<I>>() -> impl Fn(I) -> IResult<I, I, Error>
where
    I: InputTake + Compare<[u8; 4]>,
{
    bytes::complete::tag([0x4e, 0x45, 0x53, 0x1a])
}

pub fn check_signature(image: &[u8]) -> bool {
    new_signature::<&[u8], ()>()(image).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_signature() {
        let image = vec![
            0x4e, 0x45, 0x53, 0x1a, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
            0x01, 0x01,
        ];
        assert!(check_signature(&image));

        // not enough bytes
        let image = vec![0x4e, 0x45];
        assert!(!check_signature(&image));

        // invalid signature
        let image = vec![0x4e, 0x45, 0x53, 0x1b];
        assert!(!check_signature(&image));
    }
}
