@int_format = private constant [3 x i8] c"%i\00"
declare i32 @printf(i8* nocapture readonly, ...)
declare void @exit(i32)

define i32 @main() {
%1 = add nsw i32 2, 4
%2 = add nsw i32 3, %1
call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_format, i64 0, i64 0), i32 %2)
call void @exit(i32 0)
unreachable
}
