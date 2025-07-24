export function retry<T>(fn: () => Promise<T>, { count, delay }: { count: number; delay: number }): Promise<T> {
	return new Promise<T>((resolve, reject) => {
		const attempt = async (n: number) => {
			try {
				const result = await fn();
				resolve(result);
			} catch (error) {
				if (n === 1) {
					reject(error);
				} else {
					console.log(`Retrying... (${count - n + 1}/${count})`);
					console.error('Error:', error);
					setTimeout(() => attempt(n - 1), delay);
				}
			}
		};
		attempt(count);
	});
}
