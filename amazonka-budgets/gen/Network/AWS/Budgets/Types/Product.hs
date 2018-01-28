{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Product
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.Product where

import Network.AWS.Budgets.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | AWS Budget model
--
-- /See:/ 'budget' smart constructor.
data Budget = Budget'
  { _bCalculatedSpend :: !(Maybe CalculatedSpend)
  , _bCostTypes       :: !(Maybe CostTypes)
  , _bCostFilters     :: !(Maybe (Map Text [Text]))
  , _bBudgetName      :: !Text
  , _bBudgetLimit     :: !Spend
  , _bTimeUnit        :: !TimeUnit
  , _bTimePeriod      :: !TimePeriod
  , _bBudgetType      :: !BudgetType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Budget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCalculatedSpend' - Undocumented member.
--
-- * 'bCostTypes' - Undocumented member.
--
-- * 'bCostFilters' - Undocumented member.
--
-- * 'bBudgetName' - Undocumented member.
--
-- * 'bBudgetLimit' - Undocumented member.
--
-- * 'bTimeUnit' - Undocumented member.
--
-- * 'bTimePeriod' - Undocumented member.
--
-- * 'bBudgetType' - Undocumented member.
budget
    :: Text -- ^ 'bBudgetName'
    -> Spend -- ^ 'bBudgetLimit'
    -> TimeUnit -- ^ 'bTimeUnit'
    -> TimePeriod -- ^ 'bTimePeriod'
    -> BudgetType -- ^ 'bBudgetType'
    -> Budget
budget pBudgetName_ pBudgetLimit_ pTimeUnit_ pTimePeriod_ pBudgetType_ =
  Budget'
  { _bCalculatedSpend = Nothing
  , _bCostTypes = Nothing
  , _bCostFilters = Nothing
  , _bBudgetName = pBudgetName_
  , _bBudgetLimit = pBudgetLimit_
  , _bTimeUnit = pTimeUnit_
  , _bTimePeriod = pTimePeriod_
  , _bBudgetType = pBudgetType_
  }


-- | Undocumented member.
bCalculatedSpend :: Lens' Budget (Maybe CalculatedSpend)
bCalculatedSpend = lens _bCalculatedSpend (\ s a -> s{_bCalculatedSpend = a});

-- | Undocumented member.
bCostTypes :: Lens' Budget (Maybe CostTypes)
bCostTypes = lens _bCostTypes (\ s a -> s{_bCostTypes = a});

-- | Undocumented member.
bCostFilters :: Lens' Budget (HashMap Text [Text])
bCostFilters = lens _bCostFilters (\ s a -> s{_bCostFilters = a}) . _Default . _Map;

-- | Undocumented member.
bBudgetName :: Lens' Budget Text
bBudgetName = lens _bBudgetName (\ s a -> s{_bBudgetName = a});

-- | Undocumented member.
bBudgetLimit :: Lens' Budget Spend
bBudgetLimit = lens _bBudgetLimit (\ s a -> s{_bBudgetLimit = a});

-- | Undocumented member.
bTimeUnit :: Lens' Budget TimeUnit
bTimeUnit = lens _bTimeUnit (\ s a -> s{_bTimeUnit = a});

-- | Undocumented member.
bTimePeriod :: Lens' Budget TimePeriod
bTimePeriod = lens _bTimePeriod (\ s a -> s{_bTimePeriod = a});

-- | Undocumented member.
bBudgetType :: Lens' Budget BudgetType
bBudgetType = lens _bBudgetType (\ s a -> s{_bBudgetType = a});

instance FromJSON Budget where
        parseJSON
          = withObject "Budget"
              (\ x ->
                 Budget' <$>
                   (x .:? "CalculatedSpend") <*> (x .:? "CostTypes") <*>
                     (x .:? "CostFilters" .!= mempty)
                     <*> (x .: "BudgetName")
                     <*> (x .: "BudgetLimit")
                     <*> (x .: "TimeUnit")
                     <*> (x .: "TimePeriod")
                     <*> (x .: "BudgetType"))

instance Hashable Budget where

instance NFData Budget where

instance ToJSON Budget where
        toJSON Budget'{..}
          = object
              (catMaybes
                 [("CalculatedSpend" .=) <$> _bCalculatedSpend,
                  ("CostTypes" .=) <$> _bCostTypes,
                  ("CostFilters" .=) <$> _bCostFilters,
                  Just ("BudgetName" .= _bBudgetName),
                  Just ("BudgetLimit" .= _bBudgetLimit),
                  Just ("TimeUnit" .= _bTimeUnit),
                  Just ("TimePeriod" .= _bTimePeriod),
                  Just ("BudgetType" .= _bBudgetType)])

-- | A structure that holds the actual and forecasted spend for a budget.
--
-- /See:/ 'calculatedSpend' smart constructor.
data CalculatedSpend = CalculatedSpend'
  { _csForecastedSpend :: !(Maybe Spend)
  , _csActualSpend     :: !Spend
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CalculatedSpend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csForecastedSpend' - Undocumented member.
--
-- * 'csActualSpend' - Undocumented member.
calculatedSpend
    :: Spend -- ^ 'csActualSpend'
    -> CalculatedSpend
calculatedSpend pActualSpend_ =
  CalculatedSpend'
  {_csForecastedSpend = Nothing, _csActualSpend = pActualSpend_}


-- | Undocumented member.
csForecastedSpend :: Lens' CalculatedSpend (Maybe Spend)
csForecastedSpend = lens _csForecastedSpend (\ s a -> s{_csForecastedSpend = a});

-- | Undocumented member.
csActualSpend :: Lens' CalculatedSpend Spend
csActualSpend = lens _csActualSpend (\ s a -> s{_csActualSpend = a});

instance FromJSON CalculatedSpend where
        parseJSON
          = withObject "CalculatedSpend"
              (\ x ->
                 CalculatedSpend' <$>
                   (x .:? "ForecastedSpend") <*> (x .: "ActualSpend"))

instance Hashable CalculatedSpend where

instance NFData CalculatedSpend where

instance ToJSON CalculatedSpend where
        toJSON CalculatedSpend'{..}
          = object
              (catMaybes
                 [("ForecastedSpend" .=) <$> _csForecastedSpend,
                  Just ("ActualSpend" .= _csActualSpend)])

-- | This includes the options for getting the cost of a budget.
--
-- /See:/ 'costTypes' smart constructor.
data CostTypes = CostTypes'
  { _ctUseAmortized             :: !(Maybe Bool)
  , _ctIncludeRecurring         :: !(Maybe Bool)
  , _ctUseBlended               :: !(Maybe Bool)
  , _ctIncludeSupport           :: !(Maybe Bool)
  , _ctIncludeDiscount          :: !(Maybe Bool)
  , _ctIncludeSubscription      :: !(Maybe Bool)
  , _ctIncludeRefund            :: !(Maybe Bool)
  , _ctIncludeUpfront           :: !(Maybe Bool)
  , _ctIncludeOtherSubscription :: !(Maybe Bool)
  , _ctIncludeTax               :: !(Maybe Bool)
  , _ctIncludeCredit            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CostTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctUseAmortized' - A boolean value whether to include amortized costs in the cost budget.
--
-- * 'ctIncludeRecurring' - A boolean value whether to include recurring costs in the cost budget.
--
-- * 'ctUseBlended' - A boolean value whether to use blended costs in the cost budget.
--
-- * 'ctIncludeSupport' - A boolean value whether to include support costs in the cost budget.
--
-- * 'ctIncludeDiscount' - A boolean value whether to include discounts in the cost budget.
--
-- * 'ctIncludeSubscription' - A boolean value whether to include subscriptions in the cost budget.
--
-- * 'ctIncludeRefund' - A boolean value whether to include refunds in the cost budget.
--
-- * 'ctIncludeUpfront' - A boolean value whether to include upfront costs in the cost budget.
--
-- * 'ctIncludeOtherSubscription' - A boolean value whether to include other subscription costs in the cost budget.
--
-- * 'ctIncludeTax' - A boolean value whether to include tax in the cost budget.
--
-- * 'ctIncludeCredit' - A boolean value whether to include credits in the cost budget.
costTypes
    :: CostTypes
costTypes =
  CostTypes'
  { _ctUseAmortized = Nothing
  , _ctIncludeRecurring = Nothing
  , _ctUseBlended = Nothing
  , _ctIncludeSupport = Nothing
  , _ctIncludeDiscount = Nothing
  , _ctIncludeSubscription = Nothing
  , _ctIncludeRefund = Nothing
  , _ctIncludeUpfront = Nothing
  , _ctIncludeOtherSubscription = Nothing
  , _ctIncludeTax = Nothing
  , _ctIncludeCredit = Nothing
  }


-- | A boolean value whether to include amortized costs in the cost budget.
ctUseAmortized :: Lens' CostTypes (Maybe Bool)
ctUseAmortized = lens _ctUseAmortized (\ s a -> s{_ctUseAmortized = a});

-- | A boolean value whether to include recurring costs in the cost budget.
ctIncludeRecurring :: Lens' CostTypes (Maybe Bool)
ctIncludeRecurring = lens _ctIncludeRecurring (\ s a -> s{_ctIncludeRecurring = a});

-- | A boolean value whether to use blended costs in the cost budget.
ctUseBlended :: Lens' CostTypes (Maybe Bool)
ctUseBlended = lens _ctUseBlended (\ s a -> s{_ctUseBlended = a});

-- | A boolean value whether to include support costs in the cost budget.
ctIncludeSupport :: Lens' CostTypes (Maybe Bool)
ctIncludeSupport = lens _ctIncludeSupport (\ s a -> s{_ctIncludeSupport = a});

-- | A boolean value whether to include discounts in the cost budget.
ctIncludeDiscount :: Lens' CostTypes (Maybe Bool)
ctIncludeDiscount = lens _ctIncludeDiscount (\ s a -> s{_ctIncludeDiscount = a});

-- | A boolean value whether to include subscriptions in the cost budget.
ctIncludeSubscription :: Lens' CostTypes (Maybe Bool)
ctIncludeSubscription = lens _ctIncludeSubscription (\ s a -> s{_ctIncludeSubscription = a});

-- | A boolean value whether to include refunds in the cost budget.
ctIncludeRefund :: Lens' CostTypes (Maybe Bool)
ctIncludeRefund = lens _ctIncludeRefund (\ s a -> s{_ctIncludeRefund = a});

-- | A boolean value whether to include upfront costs in the cost budget.
ctIncludeUpfront :: Lens' CostTypes (Maybe Bool)
ctIncludeUpfront = lens _ctIncludeUpfront (\ s a -> s{_ctIncludeUpfront = a});

-- | A boolean value whether to include other subscription costs in the cost budget.
ctIncludeOtherSubscription :: Lens' CostTypes (Maybe Bool)
ctIncludeOtherSubscription = lens _ctIncludeOtherSubscription (\ s a -> s{_ctIncludeOtherSubscription = a});

-- | A boolean value whether to include tax in the cost budget.
ctIncludeTax :: Lens' CostTypes (Maybe Bool)
ctIncludeTax = lens _ctIncludeTax (\ s a -> s{_ctIncludeTax = a});

-- | A boolean value whether to include credits in the cost budget.
ctIncludeCredit :: Lens' CostTypes (Maybe Bool)
ctIncludeCredit = lens _ctIncludeCredit (\ s a -> s{_ctIncludeCredit = a});

instance FromJSON CostTypes where
        parseJSON
          = withObject "CostTypes"
              (\ x ->
                 CostTypes' <$>
                   (x .:? "UseAmortized") <*> (x .:? "IncludeRecurring")
                     <*> (x .:? "UseBlended")
                     <*> (x .:? "IncludeSupport")
                     <*> (x .:? "IncludeDiscount")
                     <*> (x .:? "IncludeSubscription")
                     <*> (x .:? "IncludeRefund")
                     <*> (x .:? "IncludeUpfront")
                     <*> (x .:? "IncludeOtherSubscription")
                     <*> (x .:? "IncludeTax")
                     <*> (x .:? "IncludeCredit"))

instance Hashable CostTypes where

instance NFData CostTypes where

instance ToJSON CostTypes where
        toJSON CostTypes'{..}
          = object
              (catMaybes
                 [("UseAmortized" .=) <$> _ctUseAmortized,
                  ("IncludeRecurring" .=) <$> _ctIncludeRecurring,
                  ("UseBlended" .=) <$> _ctUseBlended,
                  ("IncludeSupport" .=) <$> _ctIncludeSupport,
                  ("IncludeDiscount" .=) <$> _ctIncludeDiscount,
                  ("IncludeSubscription" .=) <$>
                    _ctIncludeSubscription,
                  ("IncludeRefund" .=) <$> _ctIncludeRefund,
                  ("IncludeUpfront" .=) <$> _ctIncludeUpfront,
                  ("IncludeOtherSubscription" .=) <$>
                    _ctIncludeOtherSubscription,
                  ("IncludeTax" .=) <$> _ctIncludeTax,
                  ("IncludeCredit" .=) <$> _ctIncludeCredit])

-- | Notification model. Each budget may contain multiple notifications with different settings.
--
-- /See:/ 'notification' smart constructor.
data Notification = Notification'
  { _nThresholdType      :: !(Maybe ThresholdType)
  , _nNotificationType   :: !NotificationType
  , _nComparisonOperator :: !ComparisonOperator
  , _nThreshold          :: !Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nThresholdType' - Undocumented member.
--
-- * 'nNotificationType' - Undocumented member.
--
-- * 'nComparisonOperator' - Undocumented member.
--
-- * 'nThreshold' - Undocumented member.
notification
    :: NotificationType -- ^ 'nNotificationType'
    -> ComparisonOperator -- ^ 'nComparisonOperator'
    -> Double -- ^ 'nThreshold'
    -> Notification
notification pNotificationType_ pComparisonOperator_ pThreshold_ =
  Notification'
  { _nThresholdType = Nothing
  , _nNotificationType = pNotificationType_
  , _nComparisonOperator = pComparisonOperator_
  , _nThreshold = pThreshold_
  }


-- | Undocumented member.
nThresholdType :: Lens' Notification (Maybe ThresholdType)
nThresholdType = lens _nThresholdType (\ s a -> s{_nThresholdType = a});

-- | Undocumented member.
nNotificationType :: Lens' Notification NotificationType
nNotificationType = lens _nNotificationType (\ s a -> s{_nNotificationType = a});

-- | Undocumented member.
nComparisonOperator :: Lens' Notification ComparisonOperator
nComparisonOperator = lens _nComparisonOperator (\ s a -> s{_nComparisonOperator = a});

-- | Undocumented member.
nThreshold :: Lens' Notification Double
nThreshold = lens _nThreshold (\ s a -> s{_nThreshold = a});

instance FromJSON Notification where
        parseJSON
          = withObject "Notification"
              (\ x ->
                 Notification' <$>
                   (x .:? "ThresholdType") <*> (x .: "NotificationType")
                     <*> (x .: "ComparisonOperator")
                     <*> (x .: "Threshold"))

instance Hashable Notification where

instance NFData Notification where

instance ToJSON Notification where
        toJSON Notification'{..}
          = object
              (catMaybes
                 [("ThresholdType" .=) <$> _nThresholdType,
                  Just ("NotificationType" .= _nNotificationType),
                  Just ("ComparisonOperator" .= _nComparisonOperator),
                  Just ("Threshold" .= _nThreshold)])

-- | A structure to relate notification and a list of subscribers who belong to the notification.
--
-- /See:/ 'notificationWithSubscribers' smart constructor.
data NotificationWithSubscribers = NotificationWithSubscribers'
  { _nwsNotification :: !Notification
  , _nwsSubscribers  :: !(List1 Subscriber)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationWithSubscribers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nwsNotification' - Undocumented member.
--
-- * 'nwsSubscribers' - Undocumented member.
notificationWithSubscribers
    :: Notification -- ^ 'nwsNotification'
    -> NonEmpty Subscriber -- ^ 'nwsSubscribers'
    -> NotificationWithSubscribers
notificationWithSubscribers pNotification_ pSubscribers_ =
  NotificationWithSubscribers'
  {_nwsNotification = pNotification_, _nwsSubscribers = _List1 # pSubscribers_}


-- | Undocumented member.
nwsNotification :: Lens' NotificationWithSubscribers Notification
nwsNotification = lens _nwsNotification (\ s a -> s{_nwsNotification = a});

-- | Undocumented member.
nwsSubscribers :: Lens' NotificationWithSubscribers (NonEmpty Subscriber)
nwsSubscribers = lens _nwsSubscribers (\ s a -> s{_nwsSubscribers = a}) . _List1;

instance Hashable NotificationWithSubscribers where

instance NFData NotificationWithSubscribers where

instance ToJSON NotificationWithSubscribers where
        toJSON NotificationWithSubscribers'{..}
          = object
              (catMaybes
                 [Just ("Notification" .= _nwsNotification),
                  Just ("Subscribers" .= _nwsSubscribers)])

-- | A structure that represents either a cost spend or usage spend. Contains an amount and a unit.
--
-- /See:/ 'spend' smart constructor.
data Spend = Spend'
  { _sAmount :: !Text
  , _sUnit   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Spend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAmount' - Undocumented member.
--
-- * 'sUnit' - Undocumented member.
spend
    :: Text -- ^ 'sAmount'
    -> Text -- ^ 'sUnit'
    -> Spend
spend pAmount_ pUnit_ = Spend' {_sAmount = pAmount_, _sUnit = pUnit_}


-- | Undocumented member.
sAmount :: Lens' Spend Text
sAmount = lens _sAmount (\ s a -> s{_sAmount = a});

-- | Undocumented member.
sUnit :: Lens' Spend Text
sUnit = lens _sUnit (\ s a -> s{_sUnit = a});

instance FromJSON Spend where
        parseJSON
          = withObject "Spend"
              (\ x -> Spend' <$> (x .: "Amount") <*> (x .: "Unit"))

instance Hashable Spend where

instance NFData Spend where

instance ToJSON Spend where
        toJSON Spend'{..}
          = object
              (catMaybes
                 [Just ("Amount" .= _sAmount),
                  Just ("Unit" .= _sUnit)])

-- | Subscriber model. Each notification may contain multiple subscribers with different addresses.
--
-- /See:/ 'subscriber' smart constructor.
data Subscriber = Subscriber'
  { _sSubscriptionType :: !SubscriptionType
  , _sAddress          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubscriptionType' - Undocumented member.
--
-- * 'sAddress' - Undocumented member.
subscriber
    :: SubscriptionType -- ^ 'sSubscriptionType'
    -> Text -- ^ 'sAddress'
    -> Subscriber
subscriber pSubscriptionType_ pAddress_ =
  Subscriber' {_sSubscriptionType = pSubscriptionType_, _sAddress = pAddress_}


-- | Undocumented member.
sSubscriptionType :: Lens' Subscriber SubscriptionType
sSubscriptionType = lens _sSubscriptionType (\ s a -> s{_sSubscriptionType = a});

-- | Undocumented member.
sAddress :: Lens' Subscriber Text
sAddress = lens _sAddress (\ s a -> s{_sAddress = a});

instance FromJSON Subscriber where
        parseJSON
          = withObject "Subscriber"
              (\ x ->
                 Subscriber' <$>
                   (x .: "SubscriptionType") <*> (x .: "Address"))

instance Hashable Subscriber where

instance NFData Subscriber where

instance ToJSON Subscriber where
        toJSON Subscriber'{..}
          = object
              (catMaybes
                 [Just ("SubscriptionType" .= _sSubscriptionType),
                  Just ("Address" .= _sAddress)])

-- | A time period indicating the start date and end date of a budget.
--
-- /See:/ 'timePeriod' smart constructor.
data TimePeriod = TimePeriod'
  { _tpStart :: !POSIX
  , _tpEnd   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimePeriod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpStart' - Undocumented member.
--
-- * 'tpEnd' - Undocumented member.
timePeriod
    :: UTCTime -- ^ 'tpStart'
    -> UTCTime -- ^ 'tpEnd'
    -> TimePeriod
timePeriod pStart_ pEnd_ =
  TimePeriod' {_tpStart = _Time # pStart_, _tpEnd = _Time # pEnd_}


-- | Undocumented member.
tpStart :: Lens' TimePeriod UTCTime
tpStart = lens _tpStart (\ s a -> s{_tpStart = a}) . _Time;

-- | Undocumented member.
tpEnd :: Lens' TimePeriod UTCTime
tpEnd = lens _tpEnd (\ s a -> s{_tpEnd = a}) . _Time;

instance FromJSON TimePeriod where
        parseJSON
          = withObject "TimePeriod"
              (\ x ->
                 TimePeriod' <$> (x .: "Start") <*> (x .: "End"))

instance Hashable TimePeriod where

instance NFData TimePeriod where

instance ToJSON TimePeriod where
        toJSON TimePeriod'{..}
          = object
              (catMaybes
                 [Just ("Start" .= _tpStart), Just ("End" .= _tpEnd)])
