#ifndef CLOSEDNAME_H_D77084EE_E284_44C3_9F6B_715D2D901FCA
#define CLOSEDNAME_H_D77084EE_E284_44C3_9F6B_715D2D901FCA

enum class Segment {
  INVALID = 0x00,
  LOCAL = 0x01,
  CLOSED = 0x02,
};

class ClosedName {
public:

  ClosedName() : segment(Segment::INVALID), offset(0) {}
  ClosedName(Segment segment, Offset offset)
    : segment(segment)
    , offset(offset) {}

  Segment segment;
  Offset offset;

};

#endif
