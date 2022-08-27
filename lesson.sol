pragma solidity >=0.4.22 <0.6.0;


contract BlindAuction {
  struct Bid {
    bytes32 blindedBid;
    uint deposit;
  }

  address payable public beneficiay;
  uint public biddingEnd;
  uint public revealEnd;
  bool public ended;

  mapping(address => Bid[]) public bids;

  address public highestBidder;
  uint public highestBid;

  mapping(address => uint) pendingReturns;

  event AuctionEnded(address winner, uint highestBid);

  modifier onlyBefore(uint _time) { require(now < _time); _; }
  modifier onlyAfter(uint _time) { require(now > _time); _; }

  constructor(
    uint _biddingTime,
    uint _revealTime,
    address payable _beneficiary
  ) public {
    beneficiay = _beneficiary;
    biddingEnd = now + _biddingTime;
    revealEnd = biddingEnd + _revealTime;
  }

  function bid(bytes32 _blindedBid)
      public
      payable
      onlyBefore(biddingEnd)
  {
    bids[msg.sender].push(Bid({
      blindedBid: _blindedBid,
      deposit: msg.value
    }));
  }

  
}