$tbb = 'https://www.threadingbuildingblocks.org/sites/default/files/software_releases/windows/tbb44_20160128oss_win_0.zip'
$boost = 'http://downloads.sourceforge.net/project/boost/boost/1.58.0/boost_1_58_0.7z?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fboost%2Ffiles%2Fboost%2F1.58.0%2F&ts=1462216976&use_mirror=iweb'
$tbb_target = 'tbb.zip'
$boost_target = 'boost.7z'
$web = New-Object Net.WebClient
$web.DownloadFile($tbb,$tbb_target)
$web.DownloadFile($boost,$boost_target)