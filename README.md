# cryptoTaxes
Compute your crypto taxes.

## Disclaimer

The software is provided "as is", without warranty of any kind, express or
implied, including but not limited to the warranties of merchantability,
fitness for a particular purpose and noninfringement.

Additionally, we make no warranty that:
* This software will meet your requirements.
* This software will be uninterrupted, timely, secure or error-free.
* The results from the use of this software will be effective, accurate or reliable.
* The quality of this software will meet your expectations.

In no event shall the authors or copyright holders be liable for any claim, damages or other
liability, whether in an action of contract, tort or otherwise, arising from,
out of or in connection with the software or the use or other dealings in the
software.

## Usage
[java Runtime Environment](https://www.java.com/download/) must be previously installed in your computer in order to run this program. 

Download compressed file (.zip extension for Windows or .tar.gz extension for Linux) from [Releases](https://github.com/cryptoTaxes/cryptoTaxes/releases), uncompress downloaded file and open a terminal in uncompressed folder.

Trading history files should be placed in `data/usr/yourUser/input` folders (where `yourUser` is the folder storing your user data). A `demo` user is provided along with his demo history files, so that you can copy the folder structure for new users.

In order to run the program, type command `run.bat` in Windows terminal or command `./run.sh` in Linux in terminal.

The following command line parameters can be written after the command:

* `-user=yourUser`       to run the program for user `yourUser` (whose history must be in `data/usr/yourUser/input` folder).

* `-verbosity=level`     where level is a number from 0 on, the higher the number, the more information is shown in report.

* `-currency=curr`       your base coin, where `curr` can be `euro`, `btc` or `usd`.

* `-price-provider=provider` to use different price provider, where `provider` is one of:

    * `coinmarketcap`, to use CoinMarketcCap (mostly outdated).

    * `coingecko`, to use CoinGecko (recommended).
  
* `-download-prices=yes` in order to update prices from [Coinmarketcap](coinmarketcap.com) and [BDE](www.bde.es).

* `-accounting-method=method` where `method` is either `fifo` or `lifo`.

* `-decimal-places=n` to use `n` decimal places when showing real numbers.

* `-price-calculation=method` to use different methods in order to compute daily prices in your currency, where `method` can be one of:

    * `open`, to use daily open price of the market.

    * `close`, to use daily close price of the market.

    * `openClose`, to use average of daily open and close prices of the market.

    * `high`, to use highest daily price of the market.

    * `low`, to use lowest daily price of the market.

    * `highLow`, to use average of daily high and low prices of the market.

* `-time-zone=z` to use `z` as your time zone for date/time conversions.

* `-year=y` to only process operations for year `y`. Assumes previous years have been processed before.

* `-parity-priorities=file` file (in `data/config` folder) defining priorities for choosing proxies in order to calculate prices in your base coin.

* `-funding-fees=yes` to account for deposits and withdrawal fees on exchangers.

After running the program, output reports will be produced in folder `data/usr/yourUser/output`.

## Configuration

The program can be further configured by editing text files in `data/config` folder.



