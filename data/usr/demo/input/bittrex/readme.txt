Place in this folder your fullOrders.csv files (corresponding to the old
csv format used by Bittrex from 2014 till 2017) and your BittrexOrderHistory_year.csv
files (corresponding to the new csv format used by Bittrex from 2018 onwards).

You can recognize the old format by checking the header which is:
  OrderUuid,Exchange,Type,Quantity,Limit,CommissionPaid,Price,Opened,Closed
The new format header is :
  Uuid,Exchange,TimeStamp,OrderType,Limit,Quantity,QuantityRemaining,Commission,Price,PricePerUnit,IsConditional,Condition,ConditionTarget,ImmediateOrCancel,Closed

Notice also that old files use UTF-16LE text format while new ones use UTF-8.

These files can be downloaded from your Bittrex account:
   Orders tab -> My Orders History -> Download History button.
