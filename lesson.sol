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

  
}