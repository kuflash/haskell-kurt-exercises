import Control.Applicative
import Control.Monad

data Name = Name {firstName :: String, lastName :: String}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophmore | Junior | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student {studentId :: Int, gradeLevel :: GradeLevel, studentName :: Name} deriving (Show)

data Teacher = Teacher {teacherId :: Int, teacherName :: Name} deriving (Show)

data Course = Course {courseId :: Int, courseTitle :: String, teacher :: Int} deriving (Show)

data Enrollment = Enrollment {student :: Int, course :: Int} deriving (Show)

data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)
  | HINQEMPTY

startWith :: Char -> String -> Bool
startWith char string = char == head string

_select :: Monad m => (a -> b) -> m a -> m b
_select prop values = do
  value <- values
  return (prop value)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test values = do
  value <- values
  guard (test value)
  return value

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
  return dpairs

_hinq selectQuery joinQuery whereQuery =
  ( \joinData ->
      (\whereResult -> selectQuery whereResult)
        (whereQuery joinData)
  )
    joinQuery

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

students :: [Student]
students =
  [ Student 1 Senior (Name "Jack" "Lord"),
    Student 2 Junior (Name "Lesley" "Silko"),
    Student 3 Freshman (Name "Debra" "Batler"),
    Student 4 Senior (Name "Tom" "Debor"),
    Student 5 Sophmore (Name "Michael" "Yard"),
    Student 6 Junior (Name "Julia" "Christ")
  ]

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Simona" "de Bovuar"),
    Teacher 200 (Name "Susen" "Zontag")
  ]

courses :: [Course]
courses =
  [ Course 101 "French language" 100,
    Course 201 "English language" 200
  ]

enrollments :: [Enrollment]
enrollments =
  [ Enrollment 1 101,
    Enrollment 2 101,
    Enrollment 2 201,
    Enrollment 3 101,
    Enrollment 4 201,
    Enrollment 4 101,
    Enrollment 5 101,
    Enrollment 6 201
  ]

query1 :: HINQ [] (Teacher, Course) Name
query1 =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English language") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher possibleCourse teacherId teacher)
    (_where ((== "French language") . courseTitle . snd))

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher missingCourse teacherId teacher)
    (_where ((== "French language") . courseTitle . snd))

studentsEnrollmentsQ =
  HINQ_
    (_select (\(st, en) -> (studentName st, course en)))
    (_join students enrollments studentId student)

studentsEnrollments :: [(Name, Int)]
studentsEnrollments = runHINQ studentsEnrollmentsQ

englishStudentsQ =
  HINQ
    (_select (fst . fst))
    (_join studentsEnrollments courses snd courseId)
    (_where ((== "English language") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery =
      HINQ
        (_select (fst . fst))
        (_join studentsEnrollments courses snd courseId)
        (_where ((== courseName) . courseTitle . snd))

instance (Semigroup (m a)) => Semigroup (HINQ m a b) where
  (HINQ s1 j1 w1) <> (HINQ s2 j2 w2) = HINQ s1 (j1 <> j2) (w1 <> w2)
  (HINQ s1 j1 w1) <> (HINQ_ s2 j2) = HINQ s1 (j1 <> j2) w1
  (HINQ_ s1 j1) <> (HINQ s2 j2 w2) = HINQ s1 (j1 <> j2) w2
  (HINQ_ s1 j1) <> (HINQ_ s2 j2) = HINQ_ s1 (j1 <> j2)
  (HINQ s1 j1 w1) <> HINQEMPTY = HINQ s1 j1 w1
  HINQEMPTY <> (HINQ s2 j2 w2) = HINQ s2 j2 w2
  (HINQ_ s1 j1) <> HINQEMPTY = HINQ_ s1 j1
  HINQEMPTY <> (HINQ_ s2 j2) = HINQ_ s2 j2
  HINQEMPTY <> HINQEMPTY = HINQEMPTY

instance (Semigroup (m a), Semigroup (m b)) => Monoid (HINQ m a b) where
  mempty = HINQEMPTY
  mappend = (<>)

englishTeachers :: HINQ [] (Teacher, Course) Name
englishTeachers =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English language") . courseTitle . snd))

frenchTeachers :: HINQ [] (Teacher, Course) Name
frenchTeachers =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "French language") . courseTitle . snd))

teachersWithCourses = runHINQ (englishTeachers <> frenchTeachers)
