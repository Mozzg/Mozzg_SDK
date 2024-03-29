unit cuConsts;

interface

const
  EmptyString = '';
  PRECISION_TIMER_MINIMUM_RESOLUTION = 1;

  EXCEPTION_MESSAGE_TIMER_NOT_AVAILABLE = 'Error creating %s, high resolution timer is not available';
  EXCEPTION_MESSAGE_TIMER_NOT_INITIALIZED = 'Error, high precision timer was not initialized';
  EXCEPTION_MESSAGE_TIMER_WAIT_FAILED = 'Error, failed wait for waitable timer';

  EXCEPTION_MESSAGE_LIST_ITEM_ALREADY_OWNED = 'Error, list item is owned by another list';
  EXCEPTION_MESSAGE_LIST_ITEM_ALREADY_OWNED_ADDING = 'Error, list item is already owned when adding to the list';
  EXCEPTION_MESSAGE_LIST_ITEM_NOT_OWNED = 'Error, list item is not owned';

  EXCEPTION_MESSAGE_HASH_ITEMS_NOT_FOUND = 'Item with name %s not found';
  EXCEPTION_MESSAGE_HASH_ITEMS_ALREADY_EXISTS = 'Item with name %s already exists';
  EXCEPTION_MESSAGE_HASH_ITEMS_COMPARE_MISSING = '%s: Object compare function not found';

  EXCEPTION_MESSAGE_STREAM_WRONG_SIZE = 'New stream size is out of bounds';
  EXCEPTION_MESSAGE_STREAM_WRONG_OFFSET = 'Stream offset is out of bounds';
  EXCEPTION_MESSAGE_STREAM_OUT_OF_BOUNDS_READ = 'Stream out of bounds on reading (%s)';
  EXCEPTION_MESSAGE_STREAM_OUT_OF_BOUNDS_WRITE = 'Stream out of bounds on writing (%s)';
  EXCEPTION_MESSAGE_STREAM_WRONG_ENCODING = 'Stream has wrong encoding';

  EXCEPTION_MESSAGE_XML_INVALID_FORMAT = 'Invalid XML format on line %d';
  EXCEPTION_MESSAGE_XML_FAILED_LOAD_FILE = 'Failed to load file "%s" with message: %s';
  EXCEPTION_MESSAGE_XML_FAILED_READ_ENCODING = 'Failed to find out XML encoding';
  EXCEPTION_MESSAGE_XML_UNKNOWN_ENCODING = 'Failed to find encoding codepage for "%s"';
  EXCEPTION_MESSAGE_XML_DIFFERENT_HEADER_BOM_ENCODING = 'Encoding in header (%s) is different from encoding in BOM (%s)';

  EXCEPTION_MESSAGE_THREAD_INVALID_MAX_EVENT_ID = 'Error initializing %s, wrong max event ID (%d)';
  EXCEPTION_MESSAGE_THREAD_INVALID_EVENT_ID = 'Event ID out of bounds (%d) in %s';
  EXCEPTION_MESSAGE_THREAD_EVENT_ID_NOT_ASSIGNED = 'Event with ID=%d is not assigned in %s';
  //EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_EVENTS_EMPTY = 'Error waiting for events in %s. Events array is empty.';
  EXCEPTION_MESSAGE_THREAD_EVENT_WAIT_ERROR = 'Error waiting for events in %s. Wait failed or abandoned.';
implementation

end.
